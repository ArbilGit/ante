%WORKING_DRIVE%
cd %WORKING_DIR%

::..............................................................................

set THIS_DIR=%CD%

goto :llvm

:: . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

:llvm

mkdir llvm\build
cd llvm\build
cmake .. %LLVM_CMAKE_CONFIGURE_FLAGS%
cmake --build . %CMAKE_BUILD_FLAGS%
cmake --build . --target install %CMAKE_BUILD_FLAGS%

cd %THIS_DIR%

::..............................................................................
