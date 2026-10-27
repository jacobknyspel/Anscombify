
# Import required libraries
from pathlib import Path
import warnings
import cv2
import numpy as np
from PIL import Image

# Image upload limits
MAX_PIXELS = 40_000_000
MAX_BYTES = 20 * 1024 * 1024

# Function to orient image
def _orient(image, orientation):
    if orientation == 2:
        return cv2.flip(image, 1)
    if orientation == 3:
        return cv2.rotate(image, cv2.ROTATE_180)
    if orientation == 4:
        return cv2.flip(image, 0)
    if orientation == 5:
        return cv2.transpose(image)
    if orientation == 6:
        return cv2.rotate(image, cv2.ROTATE_90_CLOCKWISE)
    if orientation == 7:
        return cv2.flip(cv2.transpose(image), -1)
    if orientation == 8:
        return cv2.rotate(image, cv2.ROTATE_90_COUNTERCLOCKWISE)
    return image

# Function to read image
def _read_image(path, polarity):
    path = Path(path)
    if not path.is_file():
        raise ValueError("The image file is no longer available. Please upload it again.")
    if path.stat().st_size > MAX_BYTES:
        raise ValueError("The maximum image size is 20 MiB.")
    try:
        # Check dimensions before OpenCV allocates the decoded pixel buffer.
        with warnings.catch_warnings():
            warnings.simplefilter("error", Image.DecompressionBombWarning)
            with Image.open(path) as header:
                if header.format not in {"PNG", "JPEG", "WEBP", "BMP", "TIFF"}:
                    raise ValueError("Use a PNG, JPEG, WebP, BMP or TIFF image.")
                width, height = header.size
                orientation = int(header.getexif().get(274, 1))
                if width * height > MAX_PIXELS:
                    raise ValueError("The maximum image resolution is 40 megapixels.")
    except (Image.DecompressionBombWarning, Image.DecompressionBombError) as exc:
        raise ValueError("The maximum image resolution is 40 megapixels.") from exc
    except ValueError:
        raise
    except Exception as exc:
        raise ValueError("This image could not be read. Try PNG or JPEG.") from exc
    try:
        image = cv2.imdecode(np.fromfile(path, dtype=np.uint8), cv2.IMREAD_UNCHANGED)
    except cv2.error as exc:
        raise ValueError("OpenCV could not decode this image. Try PNG or JPEG.") from exc
    if image is None:
        raise ValueError("OpenCV could not decode this image. Try PNG or JPEG.")
    if image.dtype == np.uint16:
        image = np.rint(image / 257.0).astype(np.uint8)
    elif image.dtype != np.uint8:
        raise ValueError("Use an 8-bit or 16-bit integer image, rather than floating-point pixels.")
    image = _orient(image, orientation)
    if image.ndim == 2:
        image = cv2.cvtColor(image, cv2.COLOR_GRAY2BGR)
    elif image.shape[2] == 4:
        alpha = image[:, :, 3].astype(float) / 255.0
        visible = alpha > 0.1
        if not np.any(visible):
            raise ValueError("This image is fully transparent. Please use a visible drawing.")
        if polarity == "light":
            background = 0.0
        elif polarity == "dark":
            background = 255.0
        else:
            gray = cv2.cvtColor(image[:, :, :3], cv2.COLOR_BGR2GRAY)
            background = 255.0 if np.median(gray[visible]) < 128 else 0.0
        image = np.rint(image[:, :, :3] * alpha[:, :, None]
                        + background * (1 - alpha[:, :, None])).astype(np.uint8)
    elif image.shape[2] != 3:
        raise ValueError("This image has an unsupported number of colour channels.")
    return image

# Function to sample closed paths
def _sample_closed_path(points, n):
    closed = np.vstack([points, points[0]]).astype(float)
    delta = np.diff(closed, axis=0)
    lengths = np.linalg.norm(delta, axis=1)
    keep = lengths > 0
    starts, delta, lengths = closed[:-1][keep], delta[keep], lengths[keep]
    cumulative = np.r_[0.0, np.cumsum(lengths)]
    distances = np.arange(n, dtype=float) * cumulative[-1] / n
    index = np.searchsorted(cumulative[1:], distances, side="right")
    fractions = (distances - cumulative[index]) / lengths[index]
    return starts[index] + delta[index] * fractions[:, None]

# Function to extract contours and generate points
def extract_points(path, n_points=1000, polarity="auto", threshold_mode="otsu", threshold=127, blur=3, min_length=10, max_dimension=1600, preview_path=None, mask_path=None):
    n_points, blur, max_dimension = int(n_points), int(blur), int(max_dimension)
    threshold, min_length = float(threshold), float(min_length)
    if not 3 <= n_points <= 20000:
        raise ValueError("Choose between 3 and 20,000 points.")
    if polarity not in {"auto", "dark", "light"}:
        raise ValueError("Unknown foreground setting.")
    if threshold_mode not in {"otsu", "manual"}:
        raise ValueError("Unknown threshold method.")
    if blur not in {0, 3, 5, 7}:
        raise ValueError("Smoothing must be 0, 3, 5 or 7.")
    if not 0 <= threshold <= 255 or not np.isfinite(min_length) or min_length < 0:
        raise ValueError("Invalid threshold or minimum contour length.")
    if not 100 <= max_dimension <= 3000:
        raise ValueError("Choose a processing size between 100 and 3,000 pixels.")

    image = _read_image(path, polarity)
    original_height, original_width = image.shape[:2]
    scale = min(1.0, max_dimension / max(original_height, original_width))
    if scale < 1:
        image = cv2.resize(image, (max(1, round(original_width * scale)),
                                   max(1, round(original_height * scale))),
                           interpolation=cv2.INTER_AREA)
    height, width = image.shape[:2]
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    if blur:
        gray = cv2.GaussianBlur(gray, (blur, blur), 0)
    if int(gray.max()) - int(gray.min()) < 2:
        raise ValueError("The image is blank or has too little contrast to extract outlines.")

    flag = cv2.THRESH_BINARY
    if threshold_mode == "otsu":
        flag |= cv2.THRESH_OTSU
    used_threshold, light_mask = cv2.threshold(gray, threshold, 255, flag)
    if polarity == "auto":
        border = np.r_[light_mask[0, :], light_mask[-1, :],
                       light_mask[:, 0], light_mask[:, -1]]
        polarity = "dark" if np.mean(border > 0) >= 0.5 else "light"
    mask = cv2.bitwise_not(light_mask) if polarity == "dark" else light_mask
    if not np.any(mask) or np.all(mask):
        raise ValueError("No foreground/background boundary was found. Adjust the threshold or foreground setting.")
    contours, _ = cv2.findContours(mask, cv2.RETR_LIST, cv2.CHAIN_APPROX_NONE)
    eligible = []
    for original_id, contour in enumerate(contours):
        length = float(cv2.arcLength(contour, True))
        if len(contour) >= 3 and length > 0 and length >= min_length:
            eligible.append((original_id, contour[:, 0, :].astype(float), length))
    if not eligible:
        raise ValueError("No usable contours were found. Reduce minimum contour length or adjust the threshold.")
    # Prefer long contours when the point budget cannot represent every feature.
    eligible.sort(key=lambda entry: (-entry[2], entry[0]))
    found = len(eligible)
    eligible = eligible[:n_points // 3]
    lengths = np.array([entry[2] for entry in eligible])
    quota = (n_points - 3 * len(eligible)) * lengths / lengths.sum()
    counts = np.floor(quota).astype(int) + 3
    remainder = n_points - int(counts.sum())
    order = np.argsort(-(quota - np.floor(quota)), kind="stable")
    counts[order[:remainder]] += 1
    chunks = []
    for fid, ((_, contour, _), count) in enumerate(zip(eligible, counts), start=1):
        sampled = _sample_closed_path(contour, int(count))
        sampled[:, 1] = height - 1 - sampled[:, 1]
        chunks.append(np.column_stack([sampled, np.full(count, fid),
                                       np.arange(1, count + 1)]))
    points = np.concatenate(chunks, axis=0)
    if preview_path and not cv2.imwrite(str(preview_path), image):
        raise ValueError("Could not write the image preview.")
    if mask_path and not cv2.imwrite(str(mask_path), mask):
        raise ValueError("Could not write the extraction preview.")
    return {
        "points": points,
        "metadata": {
            "original_width": original_width, "original_height": original_height,
            "width": width, "height": height, "n_points": len(points),
            "contours_found": len(contours), "contours_eligible": found,
            "contours_used": len(eligible), "contours_omitted": found - len(eligible),
            "polarity": polarity, "threshold": float(used_threshold),
            "foreground_fraction": float(np.mean(mask > 0)),
        },
    }

# Function to compile coordinates for transformation/plotting with R
def extract_points_for_r(*args, **kwargs):
    result = extract_points(*args, **kwargs)
    points = result["points"]
    result["points"] = {
        name: points[:, index].tolist()
        for index, name in enumerate(("x", "y", "feature_id", "point_order"))
    }
    return result
