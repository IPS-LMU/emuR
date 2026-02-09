function [] = doPefacForEmu(options)
    % Wrap the PEFAC pitch estimator from VOICEBOX [1] for use in emuR [2]
    %
    % PEFAC estimates pitch and probability of voicing in an audio signal.
    % With this wrapper, you can get both outputs for your entire emuDB with
    % just a single command.
    %
    % Requires the VOICEBOX tool box by Mike Brooks [1] to be available on
    % Matlab’s search path.
    %
    % This function is designed to be used with the emuR [2] function
    % add_signalViaMatlab(). To use it, pass
    %   matlabFunctionName = "doPefacForEmu"
    %   oneMatlabFunctionCallPerFile = TRUE
    %   trackNames = c("pefacPitch", "pefacProbabilityOfVoicing")
    %   trackColumns = c("data[,1]", "data[,2]")
    % to add_signalViaMatlab(). You can freely modify the trackNames. You can
    % freely choose outputFileExtension.
    %
    % This wrapper function will read a WAV file from inputFilename, apply the
    % PEFAC algorithm to it and write the results to a .mat file at
    % outputFilename. This .mat file will be consumed be emuR and converted to
    % a .Rda file.
    %
    % [1] http://www.ee.ic.ac.uk/hp/staff/dmb/voicebox/voicebox.html or
    %     https://github.com/ImperialCollegeLondon/sap-voicebox/
    % [2] https://ips-lmu.github.io/EMU.html

    arguments
        options.inputFilename (1, 1) string
        options.outputFilename (1, 1) string
        options.sex (1, 1) string = "x"
        options.outputSampleRate (1, 1) double = 500 % to match Yin default
    end


    %%%%%%%%%%
    % Metadata that will be stored in the result file
    functionName = 'doPefacForEmu: Version 2025-08-05';


    %%%%%%%%%%
    % Read input WAV file
    [audioSignal, audioSampleRate] = audioread(options.inputFilename);


    %%%%%%%%%%
    % Options for pefac algorithm
    
    % Algorithm parameter defaults
    
%    p.fstep=5;              % frequency resolution of initial spectrogram (Hz)
%    p.fmax=4000;            % maximum frequency of initial spectrogram (Hz)
%    p.fres = 20;            % bandwidth of initial spectrogram (Hz)
%    p.fbanklo = 10;         % low frequency limit of log v_filterbank (Hz)
%    p.mpsmooth = 21;       % width of smoothing filter for mean power
%    % p.maxtranf = 1000;      % maximum value of tranf cost term
%    p.shortut = 7;          % max utterance length to average power of entire utterance
%    p.pefact = 1.8;         % shape factor in PEFAC filter
%    p.numopt = 3;           % number of possible frequencies per frame
%    p.flim = [60 400];      % range of feasible fundamental frequencies (Hz)
%    p.w = dpwtdef;          % DP weights
%    % p.rampk = 1.1;          % constant for relative-amplitude cost term
%    % p.rampcz = 100;         % relative amplitude cost for missing peak
%    p.tmf = 2;              % median frequency smoothing interval (s)
%    p.tinc = 0.01;          % default frame increment (s)
%    p.sopt = 'ilcwpf';      % spectrogram options


    %%%%%%%%%%
    % Run pefac calculation

    %Outputs: fx(nframe)     Estimated pitch (Hz)
    %         tx(nframe)     Time at the centre of each frame (seconds).
    %         pv(nframe)     Probability of the frame of being voiced
    %         fv             structure containing feature vectors
    %                            fv.vuvfea(nframe,2) = voiced/unvoiced GMM features
    %
    [fx,tx,pv,fv] = v_fxpefac(audioSignal, audioSampleRate, 1/options.outputSampleRate, pefacmode);
    %data = [fx pv];
    
    estimatedPitch = fx
    probabilityOfVoicing = pv
    data = table(fx, pv)

    sampleRate = options.outputSampleRate;
    startTime = tx(1);
    units = str2mat('Hz','normalized');

    %maybe store parameters in private if ever used
    comment = ['v_fxpefac from VOICEBOX. Default mode and algorithm parameters' '\r\n' 'audio signal: ' 'audio' '\r\n'];
    comment = ['<Comment functionName="' functionName '" executionTime="' datestr(now,0) '">' crlf comment '</Comment>' crlf];


    %%%%%%%%%%
    % Save result
    units = convertStringsToChars(units);
    comment = convertStringsToChars(comment);
    % save(outfile,'data','samplerate','descriptor','unit','t0','comment'); % Phil’s original. What about descriptor? Shouldn’t it be matrix column names or something like that?
    save(options.outputFilename, "data", "sampleRate", "startTime", "units", "comment");
end
