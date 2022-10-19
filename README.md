# calibrationapps
INSTRUMENT/ EQUIPMENT CALIBRATION RECOMMENDER NOTIFIER is :
Application which log equipment calibration validation and verification report as part of Calibration Notification Management System
Instrument/ Equipment post checking or workshop calibration will be cross referenced with Equipment Main Database to evaluate and validate 
the observation operating behaviour characteristic of target Instrument/ Equipment and get Further Recommendation Notification

(1-Pass Calibration, 2-Required further Recalibration, 3-Defective Instrument/Equipment)
- Equipment Main Database will be saved in  "instrumentdb.txt" file
- Observation Logs Report saved in "calibrationobservationlog.txt", 
- Observation Log contain of Instrument/ Equipment Name, Operating Observation Behaviour Characteristic, and date of registered. 
- Further Application will check and validate the observation report  through comparation Main Database to identify the recommended strategy
- Future ongoing works : integration with IPFS, Integration with AI - Aggregated Anomaly & Drift Detection Model, Blockchain dBFT Validator
                         Front End Development in Web3/ Android/ IOS

To Run the application : Run the command "cabal run calibrationapps"
