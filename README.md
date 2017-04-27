# distributedCompute
## Evaluate a function on multiple arguments over many client machines
### (To use with MATLAB on windows machines)

   Use distributedCompute to evaluate a function on a cell array of
   arguments on many client machines. The function accepts as arguments
   the handle of the function to execute, a cell array of arguments, a
   cell array of client machine names, a cell array of drive letters to
   use in each of the client machines, and a cell array of include paths
   which will be copied to each client that the function might depend
   upon. The function copies randomized parts of the arguments-cell array
   along with the contents of each include path to each client, process
   them there with the use of MATLAB's job scheduler to utilize all
   processor cores and keeps monitoring the progress of all the clients
   using a GUI. Once all the machines are finished executing, the method
   copies the resuls back and returns a cell array of results.

   This method requires the "psexec" program which is part of Microsoft's
   SysInternals suite. This program must be found in the current Matlab
   instance's search path. This function also requires that the Matlab
   window is running with administrator priviliges since it needs the
   permissions to open Matlab windows remotely and also to access hidden
   network shares.

   The method copies input arguments, the zipped versions of each include
   path and a copy of this function file to each client and opens Matlab
   in that client locally. This will happen irrespective of which user is
   logged on in that machine at that time. The matlab window wil just open
   in the user's screen. But the process will be assigned "low" priority
   so all normal programs and processes in that machine will continue
   running unaffected since the matlabb window will have lower priority in
   execution than the normal programs. Memory constraints might come into
   play though, since the job scheduler will internally open multiple
   isntances of matlab in turn. Communication between clients and the
   master program are done through writing "execution status.txt" text
   files in the respective folders. Please note that the instance of
   Matlab calling the distributedCompute function will not do any
   computations. If you wish to use this machine also for the computation
   then it must be included as another machine in the machine names list.

   Possible Syntax:
   
   ```
 cellArrayOfResults = distributedCompute(functionToCompute,...
                                         cellArrayOfArguments,...
                                         clientMachineNames,...
                                         targetDriveLettersCellInClientMachines,...
                                         includePathsCell);

 cellArrayOfResults = distributedCompute(functionToCompute,...
                                         cellArrayOfArguments,...
                                         clientMachineNames,...
                                         targetDriveLettersCellInClientMachines,...
                                         includePathsCell,...
                                         splitRatios,...
                                         poolSize);

```

   Input Arguments:
   
   
       functionToCompute
           Handle of the function to execute. This function will be
           required to accept only one argument (functions that take in
           multiple arguments can be abstracted with a wrapper function
           that take all these arguments as a single structure and call
           the real function) and return one result.

       cellArrayOfArguments
           A cell array where each element is the argument that needs to
           be passed on to the function for evaluation. This cell array
           will be randomly sorted and distributed to each client as
           equally as possible.

       clientMachineNames
           Cell array of machine names with the proper addressing (for eg.
           "\\machine1"). This cell array also determines the number of
           clients and the other arguments required. Make sure that the
           user account running this instance of matlab has admin
           priviliges in all the mentioned client machines.

       targetDriveLettersCellInClientMachines
           Cell array of drive letters to use in each corresponding client
           machines. Make sure that the hidden share for that drive is
           enabled ('C$' for c:\; it generally is unless explicitly
           disabled).

       includePathsCell
           Cell array of full include paths which the function to execute
           will need. The contents of each of these paths (including
           subfolders) will be zipped and copied to each client and
           unzipped locally there and set as paths. This reduces network
           usage during execution. NOTE: This can also contain non-cell
           array variable values, which have to be structures with two
           parameters: 'variableName' should be a string with a name of a
           variable and 'variableValue' should be its value. These
           structures will be available as part of a cell array called
           "commonvariables" that will be in the base of each client,
           which the functions may then access.

       jobSplittingRatio (optional)
           Array of integers which specifies how to divide the jobs
           between the client machines: this is useful when some client
           machines  are slower than others, requiring uneven splitting of
           the jobs to get optimal results. Default splitting will be
           ratio of ones.


   Output
   
       cellArrayOfResults
           A cell array containing the results given by evaluation of
           function with the corresponding element of the input
           arguments-cell array. The correspondence is maintained
           perfectly and when there's an error in the evaluation that
           position will be empty in this array.
           
