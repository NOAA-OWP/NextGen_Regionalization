# read headwater catchments from CSV file
import pandas as pd
df = pd.read_csv ('C:/Users/***REMOVED***/Desktop/Ngen/regionalization/data/headwater_catchments_huc01.txt',delim_whitespace=True)
print(df)

# label the catchments
layer1 = iface.activeLayer()
label = QgsPalLayerSettings()
label.enabled = True
label.fieldName = "id"
layer1.setLabeling(QgsVectorLayerSimpleLabeling(label))
layer1.setLabelsEnabled(True)
layer1.triggerRepaint()

# select headwater catchments
#layer1.select([f.id() for f in layer1.getFeatures() if f["id"] in df.catchment.tolist()])
        
# zoom in to selected features
#iface.mapCanvas().zoomToSelected()

#print("done")