function FusionIndexGUI(aSeqPaths)
% GUI which computes the fusion index of muscle cells fusing into myotubes.
%
% The fusion index is defined as the number of nuclei in myotubes divided
% by the total number of nuclei. A myotube is defined as a region of
% cytoplasm containing 2 or more nuclei, and a nucleus is said to be inside
% a region of cytoplasm if a certain fraction of the nucleus pixels overlap
% with the myotube. This analysis can for example be performed on data
% where the nuclei have been stained using DAPI and the cells has been
% stained for myosin heavy chain. Before this GUI can be used, the user
% must generate segmentations for both the myotubes and the nuclei.
%
% The GUI lets the user select which image sequences should be included in
% the analysis, which segmentations should be used as myotubes and nuclei,
% and how much of a nucleus has to overlap with a myotube for the nucleus
% to be considered to be inside the myotube. The user can also choose
% whether or not the results should be plotted. When the computations are
% finished, the function will print the fusion indices of the individual
% image sequences and two different average fusion indices. The first
% average fusion index is averaged over the images and the second is
% averaged over all nuclei pooled together.
%
% Inputs:
% aSeqPaths - Cell array with full paths for image folders that the user
%             will be able to include in the analysis.
%
% See also:
% FusionIndex

vers = GetVersions(aSeqPaths);
vers = unique([vers{:}]);

% Check that there are at least 2 versions.
if length(vers) < 2
    errordlg(['You need to segment the nuclei and the myotubes before '...
        'you can user this GUI.'], '2 segmentations are required')
    return
end

% GUI figure.
mainFigure = figure('Name', 'Fusion index analysis',...
    'NumberTitle', 'off',...
    'MenuBar', 'none',...
    'ToolBar', 'none',...
    'Units', 'normalized',...
    'Position', [0.25 0.4 0.3 0.35]);

seqDirs = FileEnd(aSeqPaths);

% Input data for SettingsPanel, used to create ui-controls.
info.Image_sequences = Setting(...
    'name', 'Image sequences',...
    'type', 'list',...
    'default', seqDirs,...
    'alternatives_basic', seqDirs,...
    'callbackfunction', @EnableOrDisableStartButton,...
    'tooltip', 'Images to include in analysis.');
info.Tube_version = Setting(...
    'name', 'Myotube version',...
    'type', 'choice',...
    'default', vers{1},...
    'alternatives_basic', vers,...
    'tooltip', 'Segmentation result with myotubes.');
info.Nuclei_version = Setting(...
    'name', 'Nuclei version',...
    'type', 'choice',...
    'default', vers{2},...
    'alternatives_basic', vers,...
    'tooltip', 'Segmentation result with nuclei.');
info.Minimum_overlap = Setting(...
    'name', 'Minimum overlap',...
    'type', 'numeric',...
    'default', 0.25,...
    'checkfunction', @(x) str2double(x) > 0 && str2double(x) <= 1,...
    'tooltip', ['Fraction of a nucleus that must be covered for '...
    'assignment to a myotube.']);
info.Plot = Setting(...
    'name', 'Plot segmentation results',...
    'type', 'check',...
    'default', false,...
    'tooltip', ['Plots myotubes and their nuclei in white, and other '...
    'objects in gray.']);

% Create a panel with all ui-objects.
sPanel = SettingsPanel(info,...
    'Parent', mainFigure,...
    'Position', [0 0.20 1 0.80],...
    'Split', 0.3,...
    'MinList', 10);

% Button to start computation.
StartButton = uicontrol(...
    'BackgroundColor', get(mainFigure, 'color'),...
    'Style', 'pushbutton',...
    'Units', 'normalized',...
    'Position', [0 0 1 0.2],...
    'String', 'Start',...
    'Callback', @StartButton_Callback,...
    'Tooltip', 'Start the analysis.');

    function StartButton_Callback(~, ~)
        % Callback which computes fusion indices and prints the results.
        
        % Get data from the settings panel.
        sequenceIndices = sPanel.GetIndex('Image_sequences');
        sequences = aSeqPaths(sequenceIndices);
        tubeVersion = sPanel.GetValue('Tube_version');
        nucleiVersion = sPanel.GetValue('Nuclei_version');
        minOverlap = sPanel.GetValue('Minimum_overlap');
        doPlot = sPanel.GetValue('Plot');
        
        % Runs FusionIndex for all image sequences. Also prints the fusion
        % indices of the individual image sequences.
        nucleiInTubes = nan(length(sequences),1);
        nucleiOutsideTubes = nan(length(sequences),1);
        for i = 1:length(sequenceIndices)
            [index, ~, ~, inTubes, outsideTubes] =...
                FusionIndex(sequences{i}, nucleiVersion, tubeVersion,...
                'MinOverlap', minOverlap,...
                'Plot', doPlot);
            nucleiInTubes(i) = length(inTubes);
            nucleiOutsideTubes(i) = length(outsideTubes);
            fprintf('Fusion index for %s = %f\n', FileEnd(sequences{i}), index)
        end
        
        % Prints the average fusion index taken either over images or over
        % all the nuclei pooled together.
        meanIndex_images = mean(nucleiInTubes ./...
            (nucleiInTubes + nucleiOutsideTubes));
        meanIndex_nuclei = sum(nucleiInTubes) /...
            sum(nucleiInTubes + nucleiOutsideTubes);
        fprintf('Average fusion index = %f\n', meanIndex_images)
        fprintf('Pooled fusion index = %f\n', meanIndex_nuclei)
    end

    function EnableOrDisableStartButton(~, ~)
        % Enables start button when image sequences have been selected.
        
        if isempty(sPanel.GetIndex('Image_sequences'))
            set(StartButton, 'Enable', 'off')
        else
            set(StartButton, 'Enable', 'on')
        end 
    end
end