import arcpy
import arcpy.mapping as mp

outDir = "C:/MyFiles/R-dev/StopCortaderiaLife/DOCS_/LC_Layouts/BatchMaps/v2"
mxd = mp.MapDocument(r"C:/MyFiles/R-dev/StopCortaderiaLife/DOCS_/LC_Layouts/LC_BaseLayout_S2Class_A3_ByTile-v2.mxd")

FIDs = [161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182,
        183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204,
        205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226,
        227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248,
        249, 250]

dpi = 350
width = 5790
height = 4094

dfs = mp.ListDataFrames(mxd)
df0 = dfs[0]
df1 = dfs[1]

lyr_a = mp.ListLayers(mxd, data_frame=df0)
lyr_b = mp.ListLayers(mxd, data_frame=df1)

i = 0
for FID_value in FIDs:
    i = i + 1
    # LAYER A ----------------------------------------------------------------

    lyr_a[2].definitionQuery = '"FID" = ' + str(FID_value)
    # print lyr_a[2].definitionQuery

    extentOfLayer = lyr_a[2].getExtent(True)  # visible extent of layer
    df0.extent = extentOfLayer
    arcpy.RefreshActiveView()  # redraw the map

    df0.scale = 35000
    arcpy.RefreshActiveView()  # redraw the map

    # LAYER B ----------------------------------------------------------------

    lyr_b[2].definitionQuery = '"FID" = ' + str(FID_value)
    # print lyr_b[2].definitionQuery

    outFile = outDir + "/LC_CortaderiaMap_S2Class_Tile_" + str(FID_value).zfill(3) + "_v2.png"
    mp.ExportToPNG(mxd, outFile, 'PAGE_LAYOUT', width, height, dpi)

    print "Finished map tile: " + str(FID_value) + " | " + str(round((float(i) / float(len(FIDs))) * 100, 1)) + "%\n"

del mxd
