# List of record tables and corresponding camera operation matrices
recordTables <- list(R1, R2, R3, R4, R5, R6)
camOps <- list(camop_round1, camop_round2, camop_round3, camop_round4, camop_round5, camop_round6)

# Create an empty list to store detection histories
DetHist_Minute_List <- list()

# Vector of names for saving separately
round_names <- paste0("DetHist_MinuteR", 1:6)

# Loop over the six datasets
for (i in 1:6) {
        DetHist_Minute_List[[i]] <- myfunc_detectionHistory_minute(
                recordTable = recordTables[[i]],
                camOp = camOps[[i]],
                species = "Bengal Fox",   # change species name if needed
                stationCol = "Station",
                speciesCol = "Species",
                recordDateTimeCol = "DateTimeOriginal",
                includeEffort = FALSE,
                scaleEffort = FALSE
        )
        
        # Optionally assign each detection history to a named object in your environment
        assign(round_names[i], DetHist_Minute_List[[i]])
}
