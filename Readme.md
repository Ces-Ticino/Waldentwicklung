

URL for the pdfs: https://map.geo.admin.ch/?lang=de&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.swissimage-product_1946&layers_timestamp=1946&E=2706307.20&N=1143878.95&zoom=8

    mogrify -format tif pdfs/*.pdf   # also specify output path "tiffs"
    convert -layers optimize -delay 100 tiffs/*.tif gif.gif

