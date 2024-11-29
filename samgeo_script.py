import os
import torch
from samgeo import SamGeo, tms_to_geotiff

out_dir = os.path.join(os.path.expanduser('~'), 'Downloads')
checkpoint = os.path.join(out_dir, 'sam_vit_h_4b8939.pth')

device = 'cuda' if torch.cuda.is_available() else 'cpu'
sam = SamGeo(
    checkpoint=checkpoint,
    model_type='vit_h',
    device=device,
    erosion_kernel=(3, 3),
    mask_multiplier=255,
    sam_kwargs=None,
)

mask = 'satellite.tif'

image = mask = "data-out/prepared1/1961.tif"
sam.generate(image, mask)

vector = 'data-tmp/segment.gpkg'
sam.tiff_to_gpkg(mask, vector, simplify_tolerance=None)




# ex 2

from samgeo.text_sam import LangSAM

sam2 = LangSAM()
text_prompt = "tree"


sam2.predict("data-out/prepared1/1961.tif", text_prompt, box_threshold=0.24, text_threshold=0.24)
