if not exist %WORKING_DIR% mkdir %WORKING_DIR%

::..............................................................................
:: download LLVM sources

if /i "%BUILD_MASTER%" == "true" (
	git clone --depth=1 %LLVM_MASTER_URL% %WORKING_DIR%\llvm-git
	move %WORKING_DIR%\llvm-git\llvm %WORKING_DIR%
) else (
	powershell "Invoke-WebRequest -Uri %LLVM_DOWNLOAD_URL% -OutFile %WORKING_DIR%\%LLVM_DOWNLOAD_FILE%"
	7z x -y %WORKING_DIR%\%LLVM_DOWNLOAD_FILE% -o%WORKING_DIR%
	7z x -y %WORKING_DIR%\llvm-%LLVM_VERSION%.src.tar -o%WORKING_DIR%
	ren %WORKING_DIR%\llvm-%LLVM_VERSION%.src llvm
	echo %WORKING_DIR%
	ls %WORKING_DIR%
)

::..............................................................................
