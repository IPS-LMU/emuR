function [] = demoSignalScalerForOneFile(options)
    % Demo function for the emuR/Matlab interface.
    %
    % This file reads in one WAV file and multiplies its samples with
    % the scaling factor provided in the arguments. It then stores a .mat file
    % that contains the fields required by emuR: data, sampleRate, startTime,
    % units, comment.
    %
    % To use this function in emuR, pass oneMatlabFunctionCallPerFile = TRUE to
    % add_signalViaMatlab().
    arguments
        options.inputFilename (1, 1) string
        options.outputFilename (1, 1) string
        options.scalingFactor (1, 1) double = 1
    end

    % Metadata that will be stored in the results file:
    % This comment should succinctly describe the signal processing your Matlab script runs.
    comment    = "Demo Signal Scaler for one file";
    % Digital audio signals do not really have a unit, so we leave this empty.
    units      = "";
    % If the signal this function produces does not start at time=0, provide an alternative start time here.
    startTime  = 0;

    % Read input WAV file
    [data, sampleRate] = audioread(options.inputFilename);
    data = data * options.scalingFactor;

    % Save result
    units = convertStringsToChars(units);
    comment = convertStringsToChars(comment);
    save(options.outputFilename, "data", "sampleRate", "startTime", "units", "comment");
end
