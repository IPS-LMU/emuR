function [] = demoSignalScalerForManyFiles(options)
    % Demo function for the emuR/Matlab interface.
    %
    % This file reads in any number of WAV files and multiplies their samples with
    % the scaling factor provided in the arguments. It then stores many .mat files
    % that contain the fields required by emuR: data, sampleRate, startTime,
    % units, comment.
    %
    % To use this function in emuR, pass oneMatlabFunctionCallPerFile = FALSE to
    % add_signalViaMatlab().
    arguments
        options.inputFilename (1, :) string
        options.outputFilename (1, :) string
        options.scalingFactor (1, :) double
    end

    for index = 1:size(options.inputFilename, 2)
        disp(index)
        disp(options.inputFilename(1, index))

        % Metadata that will be stored in the results file:
        % This comment should succinctly describe the signal processing your Matlab script runs.
        comment    = "Demo Signal Scaler for many files";
        % Digital audio signals do not really have a unit, so we leave this empty.
        units      = "";
        % If the signal this function produces does not start at time=0, provide an alternative start time here.
        startTime  = 0;

        % Read input WAV file
        [data, sampleRate] = audioread(options.inputFilename(1, index));
        data = data * options.scalingFactor(1, index);

        % Save result
        units = convertStringsToChars(units);
        comment = convertStringsToChars(comment);
        save(options.outputFilename(1, index), "data", "sampleRate", "startTime", "units", "comment");
    end
end
