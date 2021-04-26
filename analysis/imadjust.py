"""
imadjust.py - adjust image intensity range to reasonable values

usage:

python imadjust.py infile outfile
- Adjust a single image file, writing the adjusted image to outfile.

python imadjust.py indir outdir
Adjust a directory of JPEG image files, writing the adjusted images to outdir.
Won't overwrite existing files.

See adjust_image for the actual remapping code.
"""

import numpy as np
import os
import skimage
import sys

def adjust_image(im,darkvals,lightvals) :
    """adjust a single greyscale image, represented as a 2D numpy float64 array in range 0..1"""
    darkval,lightval = np.percentile(im,(2,98))
    # find dark and light intensity values in the image, using percentile to avoid outliers
    print(f"{im.shape} dark {darkval:.4f} light {lightval:.4f}",end=' ')
    if not (0.0<=darkval<=1.0 and 0.0<=lightval<=1.0) :
        raise Exception('unexpected image intensities out of 0..1 range')
    # do a linear remapping of the image intensities to a reasonable light and dark range
    newdarkval,newlightval = 0.05,0.9
    im = (im-darkval)*((newlightval-newdarkval)/(lightval-darkval)) + newdarkval
    im.clip(0.0,1.0,out=im)
    darkvals.append(darkval); lightvals.append(lightval) # save for summary stats
    return im

def adjust_image_file(in_fpath,out_fpath,darkvals,lightvals) :
    """adjust a single JPEG image file"""
    if os.path.exists(out_fpath) :
        raise Exception(f"{out_fpath} already exists")
    im = skimage.io.imread(in_fpath,as_gray=True)
    if im.dtype == np.uint8 :
        im = im/255.0
    if len(im.shape) != 2 :
        raise Exception('2-dimensional greyscale image expected')
    if im.dtype != np.float64 :
        raise Exception('float64 type expected for image intensities')
    im = adjust_image(im,darkvals,lightvals)
    if not out_fpath.startswith('statsonly') :
        # save with image quality 100 to avoid further quality loss as much as possible
        skimage.io.imsave(out_fpath,im,quality=100)

def adjust_images(in_f_or_dir,out_f_or_dir) :
    """adjust a single JPEG image file or a directory of them"""
    darkvals,lightvals = [],[]
    if not os.path.exists(in_f_or_dir) :
        raise Exception(f"input image file or directory '{in_f_or_dir}' doesn't exist")
    if os.path.isfile(in_f_or_dir) :
        adjust_image_file(in_f_or_dir,out_f_or_dir,darkvals,lightvals)
    elif os.path.isdir(in_f_or_dir) :
        if out_f_or_dir != 'statsonly' :
            os.makedirs(out_f_or_dir,exist_ok=True)
            if not os.path.isdir(out_f_or_dir) :
                raise Exception(f"output argument '{out_f_or_dir}' must be a directory")
        print(f"adjusting images in '{in_f_or_dir}', output '{out_f_or_dir}'")
        imcount = 0
        for fname in sorted(os.listdir(in_f_or_dir)) :
            if not (fname.lower().endswith('jpg') or fname.lower().endswith('jpeg')) :
                continue
            print(f"adjusting '{fname}'",end=' ')
            adjust_image_file(os.path.join(in_f_or_dir,fname),
                                    os.path.join(out_f_or_dir,fname),
                                    darkvals,lightvals)
            imcount += 1
            print()
        print(f'{imcount} images adjusted')
        if imcount > 0 :
            print(f'average dark {sum(darkvals)/imcount:.4f}, average light {sum(lightvals)/imcount:.4f}')

def main(argv) :
    if len(argv) != 3 :
        print(__doc__)
    else :
        adjust_images(argv[1],argv[2])

if __name__ == '__main__' :
    main(sys.argv)
