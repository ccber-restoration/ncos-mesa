# Volumetric water content data description


Data downloaded in the field are added to this Google Drive folder: https://drive.google.com/drive/folders/1b_G3JgG-pnD9voulFOIRsjLpg4QYhmSk,
with subfolders for the upper and lower arrays.

Each array has 6 sensors (ECH2O EC-5 Soil Moisture Sensor; https://metergroup.com/products/ech20-ec-5-soil-moisture-sensor) by METER (formerly Decagon).

Data are recorded on a METER ZL6 BasicSimple Data Logger (https://metergroup.com/products/zl6-basic/). 

Ports 1-6 record sensor data. Port 7 records battery percentage and voltage (mV).

Water content (m³/m³) is recorded at 60-minute intervals.

Measurement volume is ~0.2 L.

## Temporal coverage

Sensor was first deployed on 2025-01-29.

Download dates:

- 2025-05-14 (May)
- 2025-06-08 (June)
- 2025-07-27 (July)
- 2025-09-04 (September)
- 2025-11-11 (November)
- 2025-12-05 (December)

Known gaps:

**Upper array**
- Port 1 (one of the shallow sensors) has a data gap (recorded 0s or negative values) in July (2025-07-02 05:00:00 to 2025-07-12 06:00:00).
- The upper array had battery issues and stopped recording after 2025-09-09 20:00:00 (battery dropped from 35%, 7076 mV to 0% and 3762 mV)
   - recorded 13 more data points, but the timestamp had been reset, so not usable.
 
**Lower array**
- Port 1 (one of the shallow sensors), recorded 0s before 2025-02-18 10:00:00
-  The timestamp reset after 2025-10-29 02:00:00 (battery: 50%, 7504 mV), then started recording sensical timestamps on 2025-11-07 14:00:00 

### Analysis notes
-  Comparing VWC across soil textures is complicated... usually best compared relative to itself.
-  Lee estimated field capacity and permanent wilting point (PWP) individually for each sensor and then rescaled the VWC values to 0 = PWP and 1 = field capacity.



