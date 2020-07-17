% Function to measure the FVA with a test stimulus at several sizes. Adapt
% stimulus will be constant and 2 staircases for each test stimulus (1up
% 1down, 1down 1up) will be randomly interleaved.
function FVA_small_and_big
% -------------------------------------------------------------------------

% Flag for testing code in open plan
OpenPlan = 0;

rng('shuffle'); %Makes randomness work

% -------------------------------------------------------------------------
% Add path
if OpenPlan ~= 1
    addpath(genpath('K:/P1238/Code/FVA'))
else
    addpath(genpath('/groups/Projects/P1238/Code/FVA'))
end

% -------------------------------------------------------------------------
% Define some parameters for the experiment

adapt_size = 6;
test_sizes = [adapt_size/2,adapt_size,adapt_size*2];
stim_SF = 21; % This spatial frequency is matched for other upright FVA experiment
fixcolour = [0 0 0];
ISI = 1;
ITI = 1.5;
test_presentation = 0.2;

% -----------------------------------------------------------------

% loading mex functions for the first time can be
% extremely slow (seconds!), so we want to make sure that
% the ones we are using are loaded.
KbCheck;GetSecs;WaitSecs(0.001);

% Get the paricipant and sesion details and create save file for results

subjid = input('Enter participant number\n');
ses = 1;
% Asks if you want to do an adaptiation run (enter 'adapt') or a baseline
% run ('enter base')
yn=1;
while yn~0;
    exptype=input('adapt, baseline or noad? ', 's');
    
    if strcmp(exptype, 'adapt')==1
        cndstr='adapt_main';
        cndint=0;
        yn=0;
    elseif strcmp(exptype, 'baseline')==1
        cndstr='adapt_baseline';
        cndint=1;
        yn=0;
    elseif  strcmp(exptype, 'noad')==1
        cndstr = 'noad';
        cndint=2;
        yn=0;
    else
        disp('Error 1. Sorry, response is not correct');
    end
end

% Load adapt direction for the participant
switch cndint
    case 0
        if OpenPlan ~= 1
            var = load('K:/P1238/Code/FVA/Psychophysics/Test_Small_And_Big/adapt_dirs_big_small.mat');
        else
            var = load('/groups/Projects/P1238/Code/FVA/Psychophysics/Test_Small_And_Big/adapt_dirs_big_small.mat');
        end
        var = var.adapt_dirs_big_small;
        switch var(var==subjid,2)
            case 1
                adaptDir = 'R';
            case 2
                adaptDir = 'L';
        end
end

% Set up name for datafile
myfile = sprintf('FVA_Big_Small_%d_%s_s%d',subjid,cndstr,ses);
[~,mydir] = uiputfile(myfile,'Choose file directory');
out_file = [mydir,myfile,'.mat'];

% Check if datafile exists, if not start new exp, if it does increase
% session number by one
tempVar = 1;
while tempVar
    if exist(out_file)
        ses = ses+1;
        myfile = sprintf('FVA_Big_Small_%d_%s_s%d',subjid,cndstr,ses);
        out_file = [mydir,myfile,'.mat'];
    else
        tempVar = 0;
    end
end

% -------------------------------------------------------------------------
% Info about Stim PC CRT monitor (Mitsubishi Diamond pro 2070SB)
% Width 16 inches, Height 12 Inches, Diag. 20 Inches (40.64cm x 30.48cm)
% Resolution: 1024x768
% 25.19685px per cm = 1 deg. of vis. angle at 57cm viewing distance

display.width = 40; % monitor width in cm
display.viewDist = 57; % participant viewing distance in cm
if OpenPlan ~= 1
    display.horRes = 1024; % horizontal resolution of monitor in pixels
else
    display.horRes = 1680;
end
display.ppd = display.horRes/(2*atand((display.width/2)/display.viewDist));

% Screen to use
whichScreen = 0;

% set resolution to 1024x768 75hz
if OpenPlan ~= 1
    res = [1024 768];
    oldRes = SetResolution(whichScreen,res(1),res(2),75);
    refreshRate = 75;   % Stim pc
else
    refreshRate = 60;
end

white = WhiteIndex(whichScreen); % monitor colour value for white
black = BlackIndex(whichScreen); % monitor colour value for black
gray = (white+black)/2; % monitor colour value for gray
gray = ceil(gray);  %Turn gray value from 127.5 to 128 because the function for generating stimuli is hard coded to set background to 128

% -------------------------------------------------------------------------
% Open a full screen window -> window = ptr to screen in use
if OpenPlan ~=1
    [window, rect] = Screen('OpenWindow',whichScreen);
    load('K:/P1160/Common/GammaTables/GammaTable_diamondproTMS_newStimPC_90pt1_pc_brightness.mat');
    Screen('LoadNormalizedGammaTable', window, GammaTable); %Load in gamma table
else
    [window, rect] = Screen('OpenWindow',whichScreen,[],[0 0 1024 768]);
end
HideCursor(window); % Make sure the mouse is hidden

% -------------------------------------------------------------------------
% Set up staircase

% Rotation for the adaptor
stim.adaptation_value = 12;

% Set delta for staircase +/- starting start points from standard
% Delta = range of rotations around the standard for the test stimuli
stim.delta = 16;

% Create values of the comparison around the (unsigned) standard
%stim.comparison=flipud(stim.standard);  % comparison always in the opposite location as the standard
stim.possiblestartpoints_h=0+stim.delta;  % Possible starting rotations for comparison at start of staircase
stim.possiblestartpoints_l=0-stim.delta;

% Just doing 1up 1 downs for reversals
reversalRule = ones(length(test_sizes),2);
reversalRule2 = reversalRule;
direction = reversalRule;
direction(:,2) = direction(:,2)*-1;

numStairs=size(reversalRule,2);

% Counter for stair loop
str_cnt = 1;
for i = 1:size(reversalRule,1)
    for j = 1:1:numStairs
        
        stair(str_cnt) = StairCaseParams_radial_viewpoint_Big_Small(reversalRule(i,j),reversalRule2(i,j), stim.possiblestartpoints_h, stim.possiblestartpoints_l, direction(i,j), test_sizes(i));
        
        str_cnt=str_cnt+1;
    end
end

% Do less reversals for noad
if cndint == 2
    for i = 1:6
        stair(i).maxReversals = 6;
    end
end

%%  Main block loop

% Measure flip interval
if OpenPlan ~=1
    Screen('DrawText',window,'Estimating monitor flip interval...', 100, 100);
    Screen('DrawText',window,'(This may take up to 20s)', 100, 120);
    Screen('Flip',window);
    [halfifi,~,~] = Screen('GetFlipInterval', window, 100, 0.00005, 20);
    halfifi = halfifi/2;
else
    halfifi = 0;
end

Screen('FillRect',window, 128);
Screen('DrawText',window,'Generating textures...', 100, 100);
Screen('Flip', window);
%----------------------------------------------------------------------
% Generate counterphase adapting stimuli
sf=1; % spatial frequency of sine wave
xs=linspace(-1,1,refreshRate); % set sinewave to go from -1 to 1 in 60 steps
xsine=sin(xs*pi*sf); % create sine wave

%Pregenerate variables for speed
adaptTex = zeros(1,xsine/2);

% If we are in adapt forward condition (case adapt 1) then make those
% stimuli, if adapt asym (adapt case 2) then make left stimuli for left
% hemi or right stimuli for right hemi

for i=1:size(xsine,2)
    switch cndint
        case 0
            switch adaptDir
                case 'L'
                    
                    adaptStim = make_Upright_Fixed_SF(xsine(i), -stim.adaptation_value, display.ppd, adapt_size, stim_SF, 300, 350); % create counterphase stimuli and put in an array
                    adaptTex(i) = Screen('MakeTexture', window, adaptStim);  %Generate texture pointer
                case 'R'
                    
                    adaptStim = make_Upright_Fixed_SF(xsine(i), stim.adaptation_value, display.ppd, adapt_size, stim_SF, 300, 350); % create counterphase stimuli and put in an array
                    adaptTex(i) = Screen('MakeTexture', window, adaptStim);  %Generate texture pointer
            end
            
        case 1
            adaptStim = make_Upright_Fixed_SF(xsine(i), 0, display.ppd, adapt_size, stim_SF, 300, 350); % create counterphase stimuli and put in an array
            adaptTex(i) = Screen('MakeTexture', window, adaptStim);  %Generate texture pointer
            
        case 2
            adaptStim = make_Upright_Fixed_SF(xsine(i), 0, display.ppd, adapt_size, stim_SF, 300, 350); % create counterphase stimuli and put in an array
            adaptTex(i) = Screen('MakeTexture', window, adaptStim);  %Generate texture pointer
    end
end

% load textures into VRAM
Screen('PreloadTextures',window,adaptTex);

% ----------------------------------------------------------------
% Now wait for a mouse click to initialise the session

% wait for mouse click
clicks = 0;
% define background colour for all future flips
Screen('FillRect', window, gray);
Screen('DrawText',window,'Waiting for mouse click...', 100, 100);
Screen('Flip',window);

while clicks < 1
    clicks = GetClicks(window);
end
Screen('DrawText',window,'Waiting 5sec to start', 100, 100);
bdk_fixcross_small_draw(fixcolour, rect, window);
vb1 = Screen('Flip',window);

bdk_fixcross_small_draw(fixcolour, rect, window);
vb1 = Screen('Flip',window,vb1+5-halfifi);
% -----------------------------------------------------------------
% Main experiment loop

adaptTime = 30; % Adapt time 30s to start
done = 0; % This will be used to break out of the while loop
btc = 1; % trial counter for block
tc = 1; % Overall trial counter
%Check if all the staircases have been completed, if they have then
%stop
while ~done;
    sumOfStairs = 0;
    for i = 1:1:numel(stair);
        sumOfStairs = sumOfStairs + stair(i).done;
    end
    if sumOfStairs == numel(stair) % then we're done
        done=1;
        break;
    end
    
    % pick a staircase
    % This defines which trial type you are doing.
    scell = ceil(rand(1,1)*numel(stair));
    
    % check if this one is done, if it is, pick another one.
    if stair(scell).done == 1;
        while stair(scell).done == 1;
            scell = ceil(rand(1,1)*numel(stair));
        end
    end
    
    %Do trial stuff
    
    %Present adapting stimuli
    
    if cndint ~= 2
        
        for j = 1:adaptTime
            for k = 1:size(xsine,2)
                %Present adapting stimulus
                Screen('DrawTexture', window, adaptTex(k)); % present adapt stimulus
                bdk_fixcross_small_draw(fixcolour, rect, window);   % draw fixation
                if adaptTime == 30
                    if k == 1
                        vb1 = Screen('Flip',window);
                    else
                        vb1 = Screen('Flip',window,(vb1+(1/refreshRate)-halfifi));
                    end
                else
                    if k == 1 && j == 1
                        vb1 = Screen('Flip',window,(vb1+ITI-halfifi));
                    else
                        vb1 = Screen('Flip',window,(vb1+(1/refreshRate)-halfifi));
                    end
                end
            end
        end
        
        %Flip to grey after last adapting stimulus
        bdk_fixcross_small_draw(fixcolour, rect, window);   % draw fixation
        vb1 = Screen('Flip',window);
        
        adaptTime = 5;
    end
    
    %Make test stimul, getting the appropriate rotation from
    %the staircase
    
    comparison=make_Upright_Fixed_SF(0.5,stair(scell).stimLev, display.ppd,stair(scell).test_size,stim_SF,700,650);
    comparisonTex=Screen('MakeTexture', window, comparison);
    
    %Present test stimulus
    Screen('DrawTexture', window, comparisonTex); % draw comparison
    %bdk_fixcross_small_draw(fixcolour, rect, window);   % draw fixation
    vb1 = Screen('Flip',window, (vb1+ISI-halfifi));
    
    %Flip to grey after test presentation time
    bdk_fixcross_small_draw(fixcolour, rect, window);   % draw fixation
    vb1 = Screen('Flip',window, (vb1+test_presentation-halfifi));
    
    %Read response
    [response, rt]=bdk_ptb_read_response_viewpoint_TMS;
    bdk_fixcross_small_draw(fixcolour, rect, window);   % draw fixation
    vb1 = GetSecs;
    
    %Repeat trials on which the participant took too long to respond
    if ~isempty(response);
        stair(scell) = UpdateStairCase_TMS(stair(scell), response);
    end
    
    tc=tc+1;
    btc=btc+1;
    
    save(out_file);
    
end

%clean up
ShowCursor;
Screen('CloseAll' );
if OpenPlan ~= 1
    SetResolution(whichScreen,oldRes);
end

save(out_file); % so that we save done=1

if done
    
    % Put data in format for psychfunc fitting code
    
    b=1;
    
    
    psych_cnt=1;
    
    for j = 1:size(stair,2)
        
        psychx(:,1)=(1:1:((stair(j).trial-1)))'; % Pull out trial numbers for staircase (1-n)
        psychx(:,2)=psych_cnt;   % Assign which psychometirc function we're on (1-n)
        psychx(:,3)=0;
        psychx(:,4)=stair(j).stimuli;    % Pull out values of comparison
        psychx(:,5)=stair(j).response;   % Pull out response
        
        % put results for each staircase into structure
        psychy(b).response=psychx;
        clear psychx;
        
        if mod(b,size(reversalRule,2))==0
            psych_cnt=psych_cnt+1; % increment psychometric function counter
        end
        b=b+1;
    end
    
    % index structure y to cocatenate all data into one matrix
    
    w=1;% set start point for first staircase in variable updown
    e=size(psychy(1).response,1); % set end point for first staircase in variable updown
    for k=1:1:size(psychy,2);
        
        updown(w:e,:)=psychy(k).response;
        
        %The break is included, otherwise we get an error message as the lines
        %of code below try to index out of the matrix
        if k==size(psychy,2); % if we reach the last staircase break
            break
        end
        w=w+size(psychy(k).response,1); % set start of next staircase in variable updown
        e=e+size(psychy(k+1).response,1); % set end of next staicase in variable updown
    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %Create scase char for header in save file
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %Have to use sprintf to tell matlab we want a new line not \n printed
    for i=1:1:size(stair,2)
        scase(i,:)=[num2str(stair(i).up), 'up ', num2str(stair(i).down), sprintf('down\n')];
    end
    update_save_scase(mydir, myfile, scase, [], updown);
    update_save_scase_no_header(mydir, myfile, updown);
end
end
