function cellArrayOfResults = distributedCompute( functionToCompute,...
    cellArrayOfArguments,clientMachineNames,...
    targetDriveLettersCellInClientMachines,includePathsCell,varargin)
%DISTRIBUTEDCOMPUTE Evaluate a function on multiple arguments over many
%client machines
%   Use distributedCompute to evaluate a function on a cell array of
%   arguments on many client machines. The function accepts as arguments
%   the handle of the function to execute, a cell array of arguments, a
%   cell array of client machine names, a cell array of drive letters to
%   use in each of the client machines, and a cell array of include paths
%   which will be copied to each client that the function might depend
%   upon. The function copies randomized parts of the arguments-cell array
%   along with the contents of each include path to each client, process
%   them there with the use of MATLAB's job scheduler to utilize all
%   processor cores and keeps monitoring the progress of all the clients
%   using a GUI. Once all the machines are finished executing, the method
%   copies the resuls back and returns a cell array of results.
%
%   This method requires the "psexec" program which is part of Microsoft's
%   SysInternals suite. This program must be found in the current Matlab
%   instance's search path. This function also requires that the Matlab
%   window is running with administrator priviliges since it needs the
%   permissions to open Matlab windows remotely and also to access hidden
%   network shares.
%
%   The method copies input arguments, the zipped versions of each include
%   path and a copy of this function file to each client and opens Matlab
%   in that client locally. This will happen irrespective of which user is
%   logged on in that machine at that time. The matlab window wil just open
%   in the user's screen. But the process will be assigned "low" priority
%   so all normal programs and processes in that machine will continue
%   running unaffected since the matlabb window will have lower priority in
%   execution than the normal programs. Memory constraints might come into
%   play though, since the job scheduler will internally open multiple
%   isntances of matlab in turn. Communication between clients and the
%   master program are done through writing "execution status.txt" text
%   files in the respective folders. Please note that the instance of
%   Matlab calling the distributedCompute function will not do any
%   computations. If you wish to use this machine also for the computation
%   then it must be included as another machine in the machine names list.
%
%   Possible Syntax:
% cellArrayOfResults = distributedCompute(functionToCompute,...
%                                         cellArrayOfArguments,...
%                                         clientMachineNames,...
%                                         targetDriveLettersCellInClientMachines,...
%                                         includePathsCell);
%
% cellArrayOfResults = distributedCompute(functionToCompute,...
%                                         cellArrayOfArguments,...
%                                         clientMachineNames,...
%                                         targetDriveLettersCellInClientMachines,...
%                                         includePathsCell,...
%                                         splitRatios,...
%                                         poolSize);
%
%
%
%   Input Arguments:
%       functionToCompute
%           Handle of the function to execute. This function will be
%           required to accept only one argument (functions that take in
%           multiple arguments can be abstracted with a wrapper function
%           that take all these arguments as a single structure and call
%           the real function) and return one result.
%
%       cellArrayOfArguments
%           A cell array where each element is the argument that needs to
%           be passed on to the function for evaluation. This cell array
%           will be randomly sorted and distributed to each client as
%           equally as possible.
%
%       clientMachineNames
%           Cell array of machine names with the proper addressing (for eg.
%           "\\machine1"). This cell array also determines the number of
%           clients and the other arguments required. Make sure that the
%           user account running this instance of matlab has admin
%           priviliges in all the mentioned client machines.
%
%       targetDriveLettersCellInClientMachines
%           Cell array of drive letters to use in each corresponding client
%           machines. Make sure that the hidden share for that drive is
%           enabled ('C$' for c:\; it generally is unless explicitly
%           disabled).
%
%       includePathsCell
%           Cell array of full include paths which the function to execute
%           will need. The contents of each of these paths (including
%           subfolders) will be zipped and copied to each client and
%           unzipped locally there and set as paths. This reduces network
%           usage during execution. NOTE: This can also contain non-cell
%           array variable values, which have to be structures with two
%           parameters: 'variableName' should be a string with a name of a
%           variable and 'variableValue' should be its value. These
%           structures will be available as part of a cell array called
%           "commonvariables" that will be in the base of each client,
%           which the functions may then access.
%
%       jobSplittingRatio (optional)
%           Array of integers which specifies how to divide the jobs
%           between the client machines: this is useful when some client
%           machines  are slower than others, requiring uneven splitting of
%           the jobs to get optimal results. Default splitting will be
%           ratio of ones.
%
%
%   Output
%       cellArrayOfResults
%           A cell array containing the results given by evaluation of
%           function with the corresponding element of the input
%           arguments-cell array. The correspondence is maintained
%           perfectly and when there's an error in the evaluation that
%           position will be empty in this array.
%
%   History
%       2011-08-18: Ramraj Velmurugan, created.
%
%
%
global crashMainThread refreshdata
crashMainThread = 0; refreshdata=false;

if nargin > 1
    % function cellArrayOfResults = distributedCompute( varargin )
    % if nargin ~= 0
    %     functionToCompute = varargin{1};
    %     cellArrayOfArguments = varargin{2};
    %     clientMachineNames = varargin{3};
    %     targetDriveLettersCellInClientMachines=varargin{4};
    %     includePathsCell=varargin{5};
    try
        noOfClients = length(clientMachineNames);
        if length(targetDriveLettersCellInClientMachines) ~= noOfClients
            error('no of drive letters does not match');
        end
        noOfTasks  = length(cellArrayOfArguments);
        basetimestamp = datestr(now,30);
        varDetails = whos('cellArrayOfArguments');
        bytesizeOfArray = varDetails.bytes;
        skipToEnd = 0;
        if exist('distributedComputeDetails.mat','file')
            load('distributedComputeDetails');
            if bytesizeOfArray == bytesizeOfArray1
                answer = questdlg('Detected a saved previous distributedCompute process; do you want to continue?',...
                    'Continue previous operation','Yes','No','Yes');
                switch(answer)
                    case 'Yes'
                        skipToEnd = 1;
                        timestamp = ts1;
                    case 'No'
                        skipToEnd = 0;
                end
            end
        end
        commonvariables = {};
        cc=0;
        for ijk = 1:length(includePathsCell)
            if ~ischar(includePathsCell{ijk})
                cc=cc+1;
                commonvariables{cc} = includePathsCell{ijk};
                includePathsCell(ijk)=[];
            end
        end
        
        if skipToEnd==0
            bytesizeOfArray1 = bytesizeOfArray;
            %ts1 = timestamp;
            timestamp = cell(noOfClients,1);
            %zip each path
            for ijk = 1:length(includePathsCell)
                disp(['Zipping ',includePathsCell{ijk}]);
                zip(['includepath',num2str(ijk),'.zip'],includePathsCell{ijk});
            end
            %% the part where the tasks are assigned clients
            splitRatios = ones(noOfClients,1);
            poolsize = [];useJobScheduler = 1;
            if nargin>5
                % user specified split ratios and poolsize
                splitRatios = varargin{1};
                poolsize = varargin{2};
            end
            if nargin>7
                useJobScheduler = varargin{3};
            end
            
            splitRatios1 = round(splitRatios*10000);
            ss = sum(splitRatios1);
            for i=length(splitRatios):-1:1
                splitRatios(i) = sum(splitRatios1(1:i));
            end
            clientassignments = ss*(1e-10:1/noOfTasks:1);
            for i=1:noOfTasks
                try
                    val=find(splitRatios>clientassignments(i),1,'first');
                catch
                    val = noOfClients; %assign task to last client just in case
                end
                if isempty(val)
                    val = noOfClients;
                end
                clientassignments(i) = val;
            end
            % we want a random assignment so that we have better chance of
            % equal distribution of tasks
            [junk randSortOrder] = sort(rand(noOfTasks,1));
            clientassignments = clientassignments(randSortOrder);
            reduceClientNo = [];
            %%
            disp('Checking reachability of each client machine...');
            %check reachability
            unreachable = true;firstTimeCheck=true;
            while unreachable
                unreachable = false;
                for counter=1:noOfClients
                    clientpath =['\\',clientMachineNames{counter},'\'...
                        targetDriveLettersCellInClientMachines{counter},'$'];
                    fprintf(1,'|');
                    if exist(clientpath,'dir')~=7
                        warning(['unable to reach ',...
                            clientMachineNames{counter},...
                            '; will check again in 30SECS...']);
                        unreachable = true;
                        pause(30);break;
                        %error([clientpath ,' unreachable.']);
                    end
                    [status result] = psexecos(['\\',clientMachineNames{counter}],...
                        'ping 127.0.0.1 -n 1');
                    if status~=0 && ~firstTimeCheck
                        warning(['psexec failed to reach ',...
                            clientMachineNames{counter},...
                            '; will check again in 30 SEC...']);
                        unreachable = true;
                        pause(30);
                    end
                end
                if firstTimeCheck
                    unreachable=true;
                    firstTimeCheck = false;
                end
            end
            %%
            randval = round(10000*rand(noOfClients,1));
            redistributeStruct.functionToCompute = functionToCompute;
            redistributeStruct.clientMachineNames = clientMachineNames;
            redistributeStruct.targetDriveLettersCellInClientMachines = ...
                targetDriveLettersCellInClientMachines;
            redistributeStruct.varargin = varargin;
            try
                writeMatlabpoolinline = false;
                %                 if strcmp(questdlg('do you want to write matlabpool in mfile itself?'),'Yes')
                %                     writeMatlabpoolinline = true;
                %                 else
                %                     writeMatlabpoolinline = false;
                %                 end
                
                for counter=1:noOfClients
                    timestamp{counter} = [basetimestamp,num2str(randval(counter))];
                    clientpath =['\\',clientMachineNames{counter},'\'...
                        targetDriveLettersCellInClientMachines{counter},'$\distributedCompute\',...
                        timestamp{counter}];
                    clientTempFolders{counter} = clientpath;
                    tasksForThisClient = find(clientassignments==counter);
                    noOfTasksForThisClient = length(tasksForThisClient);
                    argumentsCell = cell(noOfTasksForThisClient,1);
                    originalPositionCell=argumentsCell;
                    counter2=0;
                    for counter3 = 1:noOfTasksForThisClient
                        argumentsCell{counter3} = ...
                            cellArrayOfArguments{tasksForThisClient(counter3)};
                        originalPositionCell{counter3} = ...
                            tasksForThisClient(counter3);
                        counter2=counter2+1;
                    end
                    %                 for counter3=1:noOfTasks
                    %                     if ~assignedAClient(randSortOrder(counter3))
                    %                         argumentsCell{end+1} = cellArrayOfArguments{randSortOrder(counter3)};
                    %                         originalPositionCell{end+1} = randSortOrder(counter3);
                    %                         counter2=counter2+1;
                    %                         assignedAClient(randSortOrder(counter3))=1;
                    %                         if counter~=noOfClients && counter2>=noOfTasksPerClient
                    %                             break;
                    %                         end
                    %                         if sum(~assignedAClient)==0
                    %                             break;
                    %                         end
                    %
                    %                     end
                    %                 end
                    if counter2==0
                        warning(['Not using client ',clientMachineNames{counter}]);
                        reduceClientNo(end+1) = counter;
                        continue;
                    end
                    estimationsSplit(counter)= counter2+1;
                    %%
                    %save arguments cell array  to this
                    %client path
                    
                    dos(['mkdir ',clientpath]);
                    clientLocalPath = [targetDriveLettersCellInClientMachines{counter},':\distributedCompute\',...
                        timestamp{counter}];
                    save([clientpath,'\distributedComputeInput'],...
                        'argumentsCell','originalPositionCell','functionToCompute',...
                        'poolsize','useJobScheduler','redistributeStruct','commonvariables');
                    for ijk = 1:length(includePathsCell)
                        copyfile(['includepath',num2str(ijk),'.zip'],clientpath);
                    end
                    copyfile([mfilename('fullpath'),'.m'],clientpath);
                    fh = fopen([clientpath,'\batch1.bat'],'w');
                    fprintf(fh,' %s: & cd \\distributedCompute\\%s\\ & matlab -r runit',targetDriveLettersCellInClientMachines{counter},timestamp{counter});
                    
                    
                    fclose(fh);
                    fh = fopen([clientpath,'\killprocess.bat'],'w');
                    fprintf(fh,'taskkill /FI "WINDOWTITLE eq distributedComputeClient%s"',timestamp{counter});
                    fprintf(fh,' & %s: & rd \\distributedCompute\\%s /s /q',targetDriveLettersCellInClientMachines{counter},timestamp{counter});
                    fclose(fh);
                    
                    
                    fh = fopen([clientpath,'\killprocess2.bat'],'w');
                    fprintf(fh,'taskkill /im:matlab.exe /f');
                    fprintf(fh,' & %s: & rd \\distributedCompute\\%s /s /q',targetDriveLettersCellInClientMachines{counter},timestamp{counter});
                    fclose(fh);
                    
                    fh=fopen([clientpath,'\runit.m'],'w');
                    %    for ijk = 1:length(includeNetworkPathsCell)
                    %       fprintf(fh,'addpath(genpath(''%s''));\n',includeNetworkPathsCell{ijk});
                    %  end
                    if writeMatlabpoolinline
                        warning('writing matlabpool line in m file itself!');
                        fprintf(fh,'matlabpool;\n');
                    end
                    for ijk = 1:length(includePathsCell)
                        fprintf(fh,'unzip(''includepath%d.zip'');',ijk);
                    end
                    fprintf(fh,'\ntry,com.mathworks.mde.desk.MLDesktop.getInstance.getMainFrame.setTitle(''distributedComputeClient%s'');end;\n',timestamp{counter});
                    fprintf(fh,'addpath(genpath(pwd));err=0;try,distributedCompute;\n');
                    fprintf(fh,'catch e, dbstop if error;err=1; \n');
                    fprintf(fh,'disp([''there was en error'',e.message]);\n');
                    fprintf(fh,'disp(''just keeping the tool open as long as master machine is sending the survival signal.'');\n');
                    fprintf(fh,'mfp=mfilename(''fullpath'');[cdtohere ~]=fileparts(mfp);cd(cdtohere);\n');
                    fprintf(fh,'fname=dir(''survivalSignal.txt'');\ntesttime=clock;nofile=false;while(1==1), \n');
                    fprintf(fh,'pause(200);try, timediff=etime(clock,datevec(fname.date));catch eee,\n');
                    fprintf(fh,'if ~nofile, testtime=clock;nofile=true;end,timediff=etime(clock,testtime);end\n');
                    fprintf(fh,'if timediff>1800\n');
                    fprintf(fh,'disp(''no signal from master, exiting in 30min...'');\n');
                    fprintf(fh,'t=timer(''TimerFcn'',''exit'',''startdelay'',3600);start(t);');
                    fprintf(fh,'break;\nend;\nend;\nend;\n');
                    fprintf(fh,'if err==0,disp(''waiting for data to be retrieved...'');mfp=mfilename(''fullpath'');[cdtohere ~]=fileparts(mfp);cd(cdtohere); while ~exist(''dataread.txt'',''file''), pause(5);end;deldir=pwd;cd(''..'');restoredefaultpath;try, rmdir(deldir,''s'');end;exit;end');
                    %  fprintf(fh,'if e==0, t2=timer(''TimerFcn'',''deldir=pwd;cd(''''..'''');restoredefaultpath;rmdir(deldir,''''s'''');exit'',''startdelay'',400);start(t2);end');
                    
                    fclose(fh);
                    disp(['Starting MATLAB in ',clientMachineNames{counter},'...']);
                    [status result] = psexecos(['\\',clientMachineNames{counter}],...
                        [clientLocalPath,'\batch1.bat']);
                    if status~=0
                        error(['psexec failed to launch matlab in ',clientMachineNames{counter}]);
                    end
                    
                end
                goAhead = true;
            catch exc
                warning('something went wrong when starting the clients; sending close siganl to all opened clients...');
                goAhead = false;
            end
            for counter2=1:length(reduceClientNo )
                clientMachineNames{reduceClientNo(counter2)} = [];
                targetDriveLettersCellInClientMachines{reduceClientNo(counter2)} = [];
                clientTempFolders{reduceClientNo(counter2)}=[];
                timestamp{reduceClientNo(counter2)} = [];
                noOfClients =noOfClients -1;
            end
            % all clients have started matlab successfully. so we can give
            % a "start" signal.
            
            for counter2 = 1:counter
                if ~isempty(clientMachineNames{counter2})
                    clientpath =['\\',clientMachineNames{counter2},'\'...
                        targetDriveLettersCellInClientMachines{counter2},'$\distributedCompute\',...
                        timestamp{counter2}];
                    save([clientpath,filesep,'startSignal.mat'],'goAhead');
                end
            end
            if ~goAhead
                rethrow(exc);
            end
            
            ts1 = timestamp;
            tp1 = clock;
            
            save('distributedComputeDetails','bytesizeOfArray1','ts1','clientTempFolders','estimationsSplit','tp1');
        end
        clientsStatus = cell(noOfClients,1);
        clientDone = zeros(noOfClients,1);
        clients = clientMachineNames;
        dataToPassToOtherFunctions.targetDriveLettersCellInClientMachines = targetDriveLettersCellInClientMachines;
        dataToPassToOtherFunctions.timestamp = timestamp;
        dataToPassToOtherFunctions.clientMachineNames = clientMachineNames;
        dataToPassToOtherFunctions.noOfClients = noOfClients;
        dataToPassToOtherFunctions.sampleData = cellArrayOfArguments{1};
        dataToPassToOtherFunctions.functionToCompute = functionToCompute;
        dataToPassToOtherFunctions.includePathsCell = includePathsCell;
        
        
        % here we infintely loop and keep checking if all the client machines
        % have finished processing. The loop uses "statusupdate" method to
        % check the status of each client and if there's an error anywhere,
        % this script will also crash. The check is done every 2 minutes (120
        % seconds).
        
        if ~exist('clientStatus','var')
            for counter=1:noOfClients
                clientpath =['\\',clientMachineNames{counter},'\'...
                    targetDriveLettersCellInClientMachines{counter},'$\distributedCompute\',...
                    timestamp{counter}];
                if isempty(timestamp{counter})
                    clientTempFolders{counter} = [];
                else
                    clientTempFolders{counter} = clientpath;
                end
            end
        end
        while 1==1
            doagain=2;
            for counter=1:noOfClients
                if isempty(clientTempFolders{counter})
                    clientTempFolders = {clientTempFolders{1:counter-1} clientTempFolders{counter+1:end}};
                    noOfClients =numel(clientTempFolders);
                    doagain = 1;
                    break;
                end
                
            end
            if doagain == 1
                continue
            else
                break;
            end
        end
        neverErrored = 1000;firstTime = 1;
        finishFraction = zeros(noOfClients,1);
        %clients=  reshape(clients,length(clientsStatus),1);
        try
            while 1==1,
                %disp('Next status check in      ');
                if crashMainThread
                    return
                end
                dstr = datestr(now);
                for timekeep = 120:-1:1
                    pause(1);
                    if refreshdata
                        refreshdata = false;
                        break;
                    end
                    checkForAndActivateProcessUpdateFigure(dataToPassToOtherFunctions);
                    if crashMainThread
                        return
                    end
                    diff= etime(clock,tp1);
                    set(findobj('tag','distributedComputeClientStatusHeading'),...
                        'string',{['Clients have been processing for ',...
                        num2str(floor(diff/3600)),' hours and ',...
                        num2str(ceil(rem(diff/60,60))),' minutes.',...
                        ' Next status check will be in ',num2str(timekeep),...
                        ' seconds.'],[ 'Status at ',dstr,' is: ']});
                    %fprintf(1,'\b\b\b\b\b%3d s',timekeep);
                    if firstTime == 1 && timekeep == 60
                        firstTime = 0;
                        break;
                    end
                end
                errored=0;
                for counter=1:noOfClients
                    try
                        clientsStatus{counter} = statusupdate(0,clientTempFolders{counter});
                    catch e
                        warning(['Couldn''t read status file in client ',clients{counter}]);
                    end
                    try
                        fh10 = fopen([clientTempFolders{counter},filesep,'survivalSignal.txt'],'w');
                        fclose(fh10);
                    catch e22
                        warning(['Couldn''t write survivalsignal to client ',clients{counter},...
                            ' because of error: ',e22.message]);
                    end
                    if strncmp(clientsStatus{counter},'done',4)==1
                        clientDone(counter) =1;
                    end
                    if strncmp(clientsStatus{counter},'error',5)==1
                        errored = 1;
                        disp('-------------------------------------------');
                        disp(['Client ',clients{counter},' has crashed!']);
                    end
                end
                if errored == 1
                    if neverErrored==0
                        error('At least one of the clients has crashed.');
                    end
                    warning(['At least one of the clients has crashed. The script ',...
                        'will continue to run, but if the errors are not rectified ',...
                        'in the respective clients within ',num2str(neverErrored),...
                        ' more status checks, this method will also fail.']);
                    neverErrored = neverErrored-1;
                end
                disp(' ');
                if sum(clientDone) == noOfClients
                    disp('All clients have finished processing!');
                    try,close(checkForAndActivateProcessUpdateFigure); end
                    break;
                else
                    status = ['Client(s) are still running. Status at ',datestr(now),' is: '];
                    
                    
                    
                    %             disp(status);
                    data = cell(noOfClients,5);
                    timeForEstimationsAll = [];totalFinished=0;
                    
                    for counter=1:noOfClients
                        %disp([clients{counter},': ',clientsStatus{counter}]);
                        
                        data{counter,1} = clients{counter};
                        %parse clientStatus for variables:
                        st =clientsStatus{counter};
                        if length(strfind(st,';'))==1
                            try
                                timeForEstimations = str2num(st(strfind(st,';')+1:strfind(st,'~')-1));
                                noFinished = str2num(st(strfind(st,'~')+1:end));
                                noSent = estimationsSplit(counter);
                                data{counter,2} = num2str(round(100*noFinished/noSent));
                                data{counter,3} = st(1:strfind(st,';')-1);
                                data{counter,4} = round(100*noFinished/(diff/60))/100;
                                data{counter,5} = num2str(timeForEstimations,'%d,');
                                finishFraction(counter) = noFinished/noSent;
                                timeForEstimationsAll = [timeForEstimationsAll(:);timeForEstimations(:)];
                                totalFinished = totalFinished +noFinished;
                            catch e
                                2;
                            end
                        else
                            data{counter,2} = '';
                            data{counter,3} = st;
                            data{counter,4} = '';
                            data{counter,5} = '';
                            
                        end
                        status = sprintf('%s\n\r%s',status,[clients{counter},': ',st]);
                    end
                    %            status = char([clients clientsStatus cellstr(repmat(char(13),noOfClients,1))]');
                    
                    set(findobj('tag','estimationProcessClientStatusTable'),'data',...
                        data);
                    
                    remaining = (1-min(finishFraction))*diff/min(finishFraction);
                    
                    if remaining == Inf, remaining = 0;end
                    %diff*(noOfTasks-totalFinished)/totalFinished;
                    estimatedremainingtime =  [num2str(floor(remaining/3600)),' hours and ',...
                        num2str(ceil(rem(remaining/60,60))),' minutes.'];
                    
                    statistics = [num2str(totalFinished),'/',num2str(noOfTasks),...
                        ' evaluations finished. Cumulative Speed: ',...
                        num2str(totalFinished/(diff/60),'%1.2f'),...
                        '/min; Fastest eval-time: ',...
                        num2str(min(timeForEstimationsAll)/60,'%1.1f'),...
                        ' min; Slowest eval took ',...
                        num2str(max(timeForEstimationsAll)/60,'%1.1f'),...
                        ' min; Estimated time remaining: ',estimatedremainingtime];
                    %set the label on the figure too
                    set(findobj('tag','estimationProcessClientStatusStats'),...
                        'string',statistics);
                    
                    %    statusupdate( sprintf('%s\n\r%s',status,statistics),...
                    %        'c:\ati\my dropbox');
                end
            end
        catch e
            2;
            ans = questdlg('hey. stuff crashed. wanna break here?');
        end
        
        %This part of the code is reached only after all the client machines
        %update their statuses to 'done' meaning execution was fully
        %successful. Here below we will load each of the clients' "out.mat"
        %output files and check each of the estimationsingles to seeif they're
        %done. If they are done then we will put them back in to the
        %estimationSet. Note that we pass the parameter matrix indices also
        %along with the estimationsingles all the time just to make sure that
        %we don't mix them up anywhere.
        %%
        err=1;
        while(err)
            try
                outputdumps=0;
                disp('Reading and merging results from all the clients');
                cellArrayOfResults = cell(1,noOfTasks);
                memoryinfo = memory;
                while memoryinfo.MemAvailableAllArrays/1024/1024<3000
                    warning(['Amount of available memory is less than 3000M',...
                        '; distributedCompute will not attempt to read ',...
                        'results until the amount of available memory is ',...
                        'more than 500M. Waiting for 2 min before checking again']);
                    pause(60*2);
                    memoryinfo = memory;
                end
                tss=datestr(now,30);
                for counter=1:noOfClients
                    whosResult = whos('cellArrayOfResults');
                    if 1==1 && (whosResult(1).bytes/1024/1024>5000 || ...
                            (outputdumps>0 && counter==noOfClients))
                        warning(['The size of output has ',...
                            'increased to more than 5000M; distributedCompute ',...
                            'will write parts of the output from the clients ',...
                            'to mat files with the name ',tss,...
                            'distributedComputeOutputPartx.mat in the current ',...
                            'workspace. Try joining them or accessing them manually']);
                        %                     if memoryinfo.MemAvailableAllArrays/1024/1024<1000 || ...
                        %                             (outputdumps>0 && counter==noOfClients)
                        %                         warning(['The amount of available memory has ',...
                        %                             'decreased to less than 1000M; distributedCompute ',...
                        %                             'will write parts of the output from the clients ',...
                        %                             'to mat files with the name ',tss,...
                        %                             'distributedComputeOutputPartx.mat in the current ',...
                        %                             'workspace. Try joining them or accessing them manually']);
                        outputdumps=outputdumps+1;
                        if outputdumps==1
                            save([tss,'distributedComputeInputs'],...
                                'functionToCompute',...
                                'cellArrayOfArguments','clientMachineNames',...
                                'targetDriveLettersCellInClientMachines',...
                                'includePathsCell','varargin');
                        end
                        disp(['Saving ',tss,'distributedComputeOutputPart',...
                            num2str(outputdumps),'.mat...']);
                        save([tss,'distributedComputeOutputPart',...
                            num2str(outputdumps)],'cellArrayOfResults',...
                            'originalIndicesOfOutputs','-v7.3');
                                        cellArrayOfResults = cell(1,noOfTasks);

                    end
                    clear outputsCell originalIndicesOfOutputs                    
                    load([clientTempFolders{counter},filesep,'distributedComputeOutput.mat']);
                    len = length(outputsCell);
                    fprintf(1,'.');
                    for counter2 = 1:len
                        if ~isempty(originalIndicesOfOutputs{counter2})
                            cellArrayOfResults{originalIndicesOfOutputs{counter2}} = ...
                                outputsCell{counter2};
                        end
                    end
                    %             try
                    %                 rmdir(clientTempFolders{counter});
                    %             catch e
                    %                 warning(['Couldn''t delete temporary folder in client ',clients{counter}]);
                    %             end                  
                end
                if outputdumps~=0
                    outputdumps=outputdumps+1;
                      disp(['Saving ',tss,'distributedComputeOutputPart',...
                            num2str(outputdumps),'.mat...']);
                        save([tss,'distributedComputeOutputPart',...
                            num2str(outputdumps)],'cellArrayOfResults',...
                            'originalIndicesOfOutputs','-v7.3');
                end
                err=0;
            catch exc
                warning(['exception occured while reading stuff, will try again in 5 min']);
                disp(exc.message);
                pause(05*60);
            end
        end
        for counter=1:noOfClients
            
            if 1==1
                tf = fopen([clientTempFolders{counter},filesep,'dataread.txt'],'w');
                fclose(tf);
            else
                warning('not sending "data has been read" signal to the clients.');
            end
        end
        2;
        try
            delete('distributedComputeDetails.mat')
        end
    catch e
        rethrow(e);
        2; disp('whoa, there''s an error.');
        fppath = mfilename('fullpath');
        dbstop('in',[fppath,'.m'],'at','290');
        2;
        3;
        4;
        5;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        3;
        6;
        7;
        8;
        9;
        
    end
    
    
else
    if nargin==1
        if ischar(functionToCompute) && strcmpi(functionToCompute,'redistribute')
            load distributedComputeInput
            %             redistributeStruct.functionToCompute = functionToCompute;
            %             redistributeStruct.clientMachineNames = clientMachineNames;
            %             redistributeStruct.targetDriveLettersCellInClientMachines = ...
            %                 targetDriveLettersCellInClientMachines;
            %             redistributeStruct.varargin = varargin;
            evalstring = ['outputsCell = ',...
                'distributedCompute(redistributeStruct.functionToCompute,',...
                'argumentsCell,redistributeStruct.clientMachineNames,',...
                'redistributeStruct.targetDriveLettersCellInClientMachines,pwd,'];
            for i=1:numel(redistributeStruct.varargin)
                switch(class(redistributeStruct.varargin{i}))
                    case 'uint16'
                    case 'uint8'
                    case 'logical'
                    case 'double'
                        evalstring=[evalstring,...
                            num2str(redistributeStruct.varargin{i}),','];
                    case 'char'
                        evalstring = [evalstring,...
                            redistributeStruct.varargin{i},','];
                    otherwise
                        error('non string or numeric varargin values not allowed');
                end
            end
            eval(evalstring);
            originalIndicesOfOutputs = originalPositionCell;
            save('distributedComputeOutput','outputsCell','originalIndicesOfOutputs');
            statusupdate(['done; 0 0 0~0']);
            
        else
            error('invalid one parameter option');
        end
    else
        timePerEstim2=[];timePerEstim1=[];
        try,
            disp('waiting for "start" siganl from master...');
            waiting= true; howlong = 0;
            while waiting
                if exist('startSignal.mat','file')
                    waiting = false;
                else
                    pause(10);
                    howlong = howlong+10;
                end
                if howlong>1800
                    disp('waited for 30 minutes with no start signal; abandoning the job!');
                    tf = fopen(['dataread.txt'],'w');
                    fclose(tf);
                    return;
                end
            end
            load startSignal;
            if ~goAhead
                % something went wrong elsewhere; abandon job
                tf = fopen(['dataread.txt'],'w');
                fclose(tf);
                return;
            end
            disp('"start" signal received... beginning the processing...');
            load distributedComputeInput;
            assignin('base','commonvariables',commonvariables);
            len = length(argumentsCell);
            if useJobScheduler
                
                SDF = java.text.SimpleDateFormat('E MMM dd H:m:s z yyyy', java.util.Locale.US);
                disp(['Total of ',num2str(len),' evaluations to be done.']);
                sched = findResource('scheduler','type','local');
                job1 = createJob();
                set(job1,'pathDependencies',strread(path, '%s', 'delimiter',';'));
                if isempty(poolsize)
                    poolSize = sched.ClusterSize;
                else
                    poolSize = poolsize;
                end
                disp(['Performing ',num2str(poolSize),' evaluations to gauge time taken']);
                statusupdate(['Performing first ',num2str(poolSize),' evaluations']);
                
                timeTaken = zeros(poolSize,1);
                
                %do 1 estimation in each client first to find out how long it
                %takes
                if len<poolSize
                    poolSize = len;
                end
                cellcell1 = cell(poolSize,1);
                k=1;
                for i=len:-1:len-poolSize+1
                    cellcell1{k} = {functionToCompute,{argumentsCell{i}},{originalPositionCell{i}}};
                    k=k+1;
                end
                originalLen = len;
                len = len-poolSize;
                createTask(job1, @evaluateFunctions, 2, cellcell1);
                submit(job1);
                %cesc{ii} = perform_estimation(cesc{ii});
                %completionStauts(ii) = 1i;
                %%
                % See how long the estimations are taking to finish..
                jobsFinished = double(0);
                timeTaken = 0.001;
                smallerTime = 0;
                
                if originalLen<=poolSize
                    %special cases where the no of estimations to be done is lesser
                    %than the no of clients allowed.
                    timeLimit =Inf;
                    fractionNeeded = 1;
                else
                    timeLimit = 300;
                    fractionNeeded = 0.75;
                end
                
                while timeTaken < timeLimit
                    pause(20);
                    [p,r,f] = job1.findTask;
                    pl = length(p);rl=length(r);fl=length(f);
                    jobsFinished = fl/(pl+rl+fl);
                    %         disp([num2str(pl),' jobs pending, ',num2str(rl),...
                    %             ' jobs running, ',num2str(fl),' jobs finished.']);
                    if jobsFinished>=fractionNeeded
                        noOfInitialEstimations = fl;
                        disp('Most of the jobs finished before 5 minutes');
                        smallerTime = timeTaken;
                        break;
                    end
                    timeTaken = timeTaken+20;
                end
                
                
                idealNoOfJobsPerTask=1;
                if originalLen<=poolSize
                    %no need to do further stuff..
                    results2 = [];
                    
                    %get the results and put them back into a new cesc array
                    % clear cesc cpmilc cellcell cellcell1
                    results1 = getAllOutputArguments(job1);
                    %results2 = getAllOutputArguments(job2);
                    
                else
                    
                    if smallerTime~=0
                        %most jobs finished within 5 min, find out mean time taken for jobs
                        %to finish and construct cell arrays of tasks accordingly.
                        %find mean time taken by finished tasks
                        meantt=  0;
                        timePerEstim1 = zeros(fl,1);
                        
                        for i=1:fl
                            startdate = f(i).StartTime;
                            finishdate = f(i).FinishTime;
                            JDATEStart = SDF.parse(startdate);
                            JDATEFinish = SDF.parse(finishdate);
                            timePerEstim1(i) = floor((JDATEFinish.getTime - JDATEStart.getTime)/1000);
                            meantt =meantt+ timePerEstim1(i);
                        end
                        meantt = meantt/fl;
                        nForMean = fl;
                        idealNoOfJobsPerTask = ceil(300/meantt);
                        if idealNoOfJobsPerTask >len
                            idealNoOfJobsPerTask = 1;
                        end
                        disp(['Each task will contain ',num2str(idealNoOfJobsPerTask),' evaluations']);
                        if len~=1
                            divisions = 1:idealNoOfJobsPerTask:len;
                            if divisions(end) ~=len, divisions(end+1) = len;end
                            noOfDivisions = length(divisions)-1;
                            
                        else
                            noOfDivisions = 1;
                            divisions = [1 1];
                        end
                        cellcell = cell(noOfDivisions,1);
                        for i=1:noOfDivisions
                            b = divisions(i);
                            e = divisions(i+1)-1;
                            if i==length(divisions)-1
                                e = len;
                            end
                            subcell = cell(b-e+1,1);k=0;
                            pmsubcell = cell(b-e+1,1);
                            for ii=b:e
                                k=k+1;
                                subcell{k} = argumentsCell{ii};
                                pmsubcell{k} = originalPositionCell{ii};
                            end
                            cellcell{i} = {functionToCompute, subcell, pmsubcell};
                        end
                        
                    else
                        %most tasks DIDNT finish within 5 min, its ok to pass single
                        %Estimations as seperate tasks.
                        disp(['Most of the initial evaluations are still running after ',...
                            '5 min. Hence, each task will be assigned just one evaluation...']);
                        cellcell = cell(len,1);
                        for i=1:len
                            cellcell{i} = {functionToCompute,{argumentsCell{i}},{originalPositionCell{i}}};
                        end
                        
                    end
                    % meantt = mean(timeTaken);
                    
                    %% create another job for all the other estimations and submit it also to the local scheduler
                    job2 = createJob();
                    set(job2,'pathDependencies',strread(path, '%s', 'delimiter',';'));
                    
                    createTask(job2, @evaluateFunctions, 2, cellcell);
                    submit(job2);
                    %% check status of job2 every minute
                    jobsDone = 0;
                    while jobsDone~=1
                        if smallerTime==0
                            %those earlier estimations are still likely running. Just check
                            %for them until they finish.
                            [p,r,f] = job1.findTask;
                            pl = length(p);rl=length(r);fl=length(f);
                            timePerEstim1 = zeros(fl,1);
                            for i=1:fl
                                startdate = f(i).StartTime;
                                finishdate = f(i).FinishTime;
                                JDATEStart = SDF.parse(startdate);
                                JDATEFinish = SDF.parse(finishdate);
                                timePerEstim1(i) = floor((JDATEFinish.getTime - JDATEStart.getTime)/1000);
                            end
                            if fl/(pl+rl+fl)~=1
                                statusupdate([num2str(pl+rl),...
                                    ' initial evaluations still running;',...
                                    num2str([timePerEstim1(:)]'),...
                                    '~',num2str(fl)]);
                            else
                                disp('Now processing the rest of the evaluations:     ');
                                smallerTime = 1;
                                results1 = getAllOutputArguments(job1);
                                noOfInitialEstimations = fl;
                                continue;
                            end
                        else
                            [p,r,f] = job2.findTask;
                            pl = length(p);rl=length(r);fl=length(f);
                            if pl~=0 || rl~=0
                                timePerEstim2 = zeros(fl,1);
                                for i=1:fl
                                    startdate = f(i).StartTime;
                                    finishdate = f(i).FinishTime;
                                    JDATEStart = SDF.parse(startdate);
                                    JDATEFinish = SDF.parse(finishdate);
                                    timePerEstim2(i) = floor((JDATEFinish.getTime - JDATEStart.getTime)/(1000*idealNoOfJobsPerTask));
                                end
                                %check for errors
                                % taskIDWithErrorCell = get( f( didError ), {'ID'} );
                                % taskIDWithError = [taskIDWithErrorCell{:}];
                                %  fprintf(1,'\b\b\b\b%3d%%', round(fl/(pl+fl+rl)));
                                statusupdate([num2str((noOfInitialEstimations+fl)*idealNoOfJobsPerTask),...
                                    ' finished, ',num2str(rl*idealNoOfJobsPerTask),...
                                    ' running, ',num2str(pl*idealNoOfJobsPerTask),...
                                    ' pending;',num2str([timePerEstim1(:); timePerEstim2(:)]'),...
                                    '~',num2str((fl+noOfInitialEstimations)*idealNoOfJobsPerTask)]);
                                disp([num2str((noOfInitialEstimations+fl)*idealNoOfJobsPerTask),...
                                    ' finished, ',num2str(rl*idealNoOfJobsPerTask),...
                                    ' running, ',num2str(pl*idealNoOfJobsPerTask),' pending']);
                                
                            else
                                %statusupdate('All estimations have finished!');
                                disp('Finished all evaluations');
                                jobsDone = 1;
                                break;
                            end
                        end
                        pause(60);
                    end
                    
                    %%
                    %get the results and put them back into a new cesc array
                    %clear cellcell cellcell1
                    results1 = getAllOutputArguments(job1);
                    results2 = getAllOutputArguments(job2);
                end
                
                %%
                outputsCell = cell(length(results1)+length(results2),1);originalIndicesOfOutputs =outputsCell;
                k=0;
                for i=1:length(results1)
                    k=k+1;
                    outputsCell{k} = results1{i,1}{1};
                    originalIndicesOfOutputs{k} = results1{i,2}{1};
                end
                for i=1:size(results2,1)
                    for j=1:length(results2{i,1})
                        k=k+1;
                        outputsCell{k} = results2{i,1}{j};
                        originalIndicesOfOutputs{k} = results2{i,2}{j};
                    end
                end
                save('distributedComputeOutput','outputsCell','originalIndicesOfOutputs');
                statusupdate(['done;',num2str([timePerEstim1(:); timePerEstim2(:)]'),...
                    '~',num2str(fl+noOfInitialEstimations*idealNoOfJobsPerTask)]);
                disp('All estimations have been finished successfully');
            else
                if matlabpool('size')==0 && poolsize~=1
                    matlabpool open
                else
                    %                 if poolsize==1
                    %                     matlabpool close
                    %                 end
                end
                
                outputsCell = cell(len,1);
                if poolsize == 1
                    fractionFree = 0;attempts = 0;fractionTrheshold = 0.8;
                    while fractionFree<fractionTrheshold
                        % check fraction of free memory
                        [USERVIEW, SYSTEMVIEW] = memory;
                        fractionFree = (SYSTEMVIEW.PhysicalMemory.Total - ...
                            SYSTEMVIEW.PhysicalMemory.Available)/...
                            SYSTEMVIEW.PhysicalMemory.Total;
                        attempts = attempts+1;
                        if attempts > 2
                            warning('memory usage not going down; still proceeding, but might crash');
                            break;
                        end
                        if fractionFree<fractionTrheshold
                            break;
                        else
                            statusupdate('waiting for memory usage to go down');
                            disp('memory usage too high to start; waiting for ten minutes.');
                            pause(10*60);
                        end
                        
                    end
                    
                    
                    statusupdate('Starting evaluations...');
                    timing=tic;timing2=tic;
                    for i=1:len
                        outputsCell{i} = functionToCompute(argumentsCell{i});
                        if toc(timing)>10
                            disp([num2str(i), ' finished, 1 running, ',...
                                num2str(len-i-1),' pending;0~',num2str(i)]);
                            statusupdate([num2str(i), ' finished, 1 running, ',...
                                num2str(len-i-1),' pending;0~',num2str(i)]);
                            timing = tic;
                            if toc(timing2)>60*60*1
                                timing2=tic;
                                save('distributedComputeOutputTemp',...
                                    'outputsCell','originalPositionCell');
                            end
                        end
                        
                    end
                else
                    statusupdate('Running evaluations with parfor~0');
                    parfor i=1:len
                        outputsCell{i} = functionToCompute(argumentsCell{i});
                        if len<25
                            %if only 25 or less estimations, give the
                            %user feedback by writing a file in each loop
                            %iteration
                            if ~exist('ex-status','dir')
                                mkdir('ex-status');
                            end
                            fh = fopen(['ex-status',filesep,datestr(now,30),'.txt'],'w');
                            fclose(fh);
                            %                         outputValue = outputsCell{i};
                            %                         originalPos = originalPositionCell{i};
                            %                         save(['ex-status',filesep,datestr(now,30)],...
                            %                             'outputValue','originalPos');
                        end
                    end
                end
                originalIndicesOfOutputs = originalPositionCell;
                save('distributedComputeOutput','outputsCell','originalIndicesOfOutputs');
                statusupdate(['done; 0 0 0~0']);
                disp('all evals finished successfully');
            end
            % % % %     %%
            % % % %
            % % % %     %     into=floor(len/9);
            % % % %     %     divs=ceil(len/into);
            % % % %     %     splits=1:divs:len
            % % % %     %     splits(end+1)=len;
            % % % %     statusupdate('Started estimations');
            % % % %     for i=1:into
            % % % %         parfor j=splits(i):splits(i+1)
            % % % %             cesc{j} = perform_estimation(cesc{j});
            % % % %         end
            % % % %         strs=['Part ',num2str(i),' of ',num2str(into),' done.'];
            % % % %         statusupdate(strs);
            % % % %         disp(strs);
            % % % %     end
            % % % %     save('out','cesc','cpmilc');
            % % % %     statusupdate('done');
            % % % %     dfisp('All estimations have been finished successfully');
        catch e
            save('dumpbeforeerror');
            statusupdate(['error: ',e.message]);
            rethrow(e);
            
        end
        cellArrayOfResults = [];
    end
end
end
function [output,order] = evaluateFunctions(fhandle,inputargs,indextopassback)
% fhandle = inputcellarray{1};
% inputargs = inputcellarray{2};
% indextopassback = inputcellarray{3};
disp(inputargs);
output = {};%sqrt(inputargs);
order = {};
for i=1:length(inputargs);
    %output{i} = fhandle(inputcellarray{2}{i});
    output{i} = fhandle(inputargs{i});
    order{i} = indextopassback{i};
end


end
function fh = checkForAndActivateProcessUpdateFigure(dataToPassToOtherFunctions)
%%
fh = findobj('tag','process_estim_singles_figure');
openFigure=0;
if isempty(fh)
    openFigure = 1;
    
else
    if length(fh)>1
        close(fh);
        openFigure=1;
    end
end

if openFigure==1
    
    fh = figure('tag','process_estim_singles_figure','position',...
        [   501   278   574   388],'toolbar','none','menubar','none',...
        'name','distributedCompute Master-client status',...
        'resize','off');
    
    margin = 0.050;labelH = 0.1;
    buttonheight = labelH*0.75;buttonwidth = 0.2;
    tbox = uitable('units','normalized',...
        'columnname',{'Client name','% done','Execution Status','evals/min','Individual Times Taken for each estimation'},...
        'position',[margin,margin*2+buttonheight,1-2*margin,1-4*margin-labelH*2-buttonheight],...
        'tag','estimationProcessClientStatusTable');
    label = uicontrol('style','text','units','normalized',...
        'position',[margin,1-3*margin-labelH,1-2*margin,labelH],...
        'string','hi','fontsize',10,'horizontalalignment','left',...
        'tag','estimationProcessClientStatusStats');
    
    label2 = uicontrol('style','text','units','normalized',...
        'position',[margin,1-2*margin,1-2*margin,labelH],...
        'string','hi2','fontsize',13,'horizontalalignment','left',...
        'tag','distributedComputeClientStatusHeading');
    button1 = uicontrol('style','pushbutton','units','normalized',...
        'position',[1-margin-buttonwidth margin buttonwidth buttonheight],...
        'string','Kill all clients','callback',{@killclients,dataToPassToOtherFunctions});
    button2 = uicontrol('style','pushbutton','units','normalized',...
        'position',[margin margin buttonwidth buttonheight],...
        'string','Refresh data now','callback',{@refreshdata,dataToPassToOtherFunctions});
    
    
    set(tbox,'units','pixels');
    tp = get(tbox','position');
    width = tp(3)-40;
    colwidths = {width*.3,width*.09,width*.44,width*0.11,width};
    set(tbox,'columnwidth',colwidths,'fontsize',13);
else
    set(0,'currentFigure',fh);
end
end
function refreshdata(fh,e,dataToPassToOtherFunctions)
global refreshdata
refreshdata =true;
end
function killclients(fh,e,dataToPassToOtherFunctions)
global crashMainThread
answ=questdlg('This will kill ALL MATLAB windows in the clients. Do you want to proceed?','Kill all clients','Yes','No','No');
switch(answ)
    case 'Yes'
        for counter = 1:dataToPassToOtherFunctions.noOfClients
            clientLocalPath = [...
                dataToPassToOtherFunctions...
                .targetDriveLettersCellInClientMachines{counter},...
                ':\distributedCompute\',dataToPassToOtherFunctions...
                .timestamp{counter}];
            disp(['sending kill signal to ',dataToPassToOtherFunctions.clientMachineNames{counter}]);
            [status result] = psexecos(['\\',...
                dataToPassToOtherFunctions.clientMachineNames{counter}],...
                [clientLocalPath,'\killprocess.bat']);
            if status~=0
                %                 answ2=questdlg(['PsKill could not kill the matlab window in ',...
                %                     dataToPassToOtherFunctions.clientMachineNames{counter},...
                %                     '; Do you want to attempt killing ALL Matlab windows ',...
                %                     ' in the machine?'],'Kill all MATLAB windows','Yes','No','No');
                %                 switch(answ2)
                %                     case 'Yes'
                disp(['sending kill-all Matlab signal to ',...
                    dataToPassToOtherFunctions.clientMachineNames{counter}]);
                [status2 result2] = psexecos(['\\',...
                    dataToPassToOtherFunctions.clientMachineNames{counter}],...
                    [clientLocalPath,'\killprocess2.bat']);
                if status2~=0
                    disp(['psexec failed to launch kill process in ',...
                        dataToPassToOtherFunctions.clientMachineNames{counter},...
                        ' during both attempts']);
                end
                
                %                     otherwise
                %                         disp(['psexec failed to launch kill process in ',...
                %                             dataToPassToOtherFunctions.clientMachineNames{counter}]);
                %                 end
                
            end
        end
        if exist('distributedComputeDetails.mat','file')
            delete('distributedComputeDetails.mat');
        end
        answ=questdlg(['Sent a kill signal to all clients; do you want',...
            ' to run a single process here to recapitulate any error?'],...
            'Run a single computation','Yes','No','No');
        if strcmp(answ,'Yes')
            crashMainThread = 1;
            dbstat = dbstatus;
            dbstop if error
            pause(4);
            drawnow;
            disp('setting path...');
            for counter=1:numel(dataToPassToOtherFunctions.includePathsCell)
                addpath(genpath(...
                    dataToPassToOtherFunctions.includePathsCell{counter}));
            end
            processResult = dataToPassToOtherFunctions.functionToCompute(...
                dataToPassToOtherFunctions.sampleData);
            assignin('base','sampleResult',processResult);
            disp(['Function Executed and returned a result; ',...
                'result stored in "sampleResult" variable in base workspace']);
            dbstop(dbstat);
        end
        try,close(checkForAndActivateProcessUpdateFigure); end
        error('User cancelled the distributedComputeProcess');
end
end
function [status result] = psexecos( computer,argumentString )
currentPath = pwd;
cd(fileparts(mfilename('fullpath')));
pathToGo = fileparts(which('psexec.exe'));
if isempty(pathToGo)
    disp('can not find psexec.exe! place the executable file in the path somewhere!');
    error('can not find psexec.exe! place that file in the same folder as this method!');
end
cd(pathToGo);
[status result] = dos(['psexec ',computer,' -i 0 -low ',argumentString]);
cd(currentPath);


end

function returnS = statusupdate( updateString,varargin )
%statusupdate( updateString ) OR statusupdate( updateString,folderPath )
% (To Check status: updateString = 0;)
%
%   Makes (or if exists, deletes and makes a new) file of the name
%   'execution_status.txt' and writes the 'updateString as the only text of
%   the file. If called with updateString as an integer of value 0, then it
%   will RETRIEVE the status from the 'execution_status.txt' file in
%   'folderPath'

if ischar(updateString)
    curDir = pwd;
    if nargin>1
        cd(varargin{1});
    end
    f = fopen('execution_status.txt','w');
    slashes = findstr(updateString,'\');
    if length(slashes)==0
        updateString2 = updateString;
    else
        updateString2 = updateString(1:slashes(1));
        slashes = [slashes length(updateString)];
        for i=1:length(slashes)-1
            updateString2 = [updateString2,...
                '\',updateString(slashes(i)+1:slashes(i+1))];
        end
        
    end
    fprintf(f,updateString2);
    fclose(f);
    cd(curDir);
else
    returnS = fileread([varargin{1},filesep,'execution_status.txt']);
    
end

end
