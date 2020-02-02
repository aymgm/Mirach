# escape=`

FROM mcr.microsoft.com/windows:1809-amd64

RUN curl https://codeload.github.com/msys2/msys2-ci-base/zip/master -o msys2-ci-base.zip && `
    powershell -Command Expand-Archive msys2-ci-base.zip && `
    powershell -Command Move-Item -Path C:\msys2-ci-base\msys2-ci-base-master C:\msys64 && `
    path C:\msys64\usr\bin;C:\msys64\mingw64\bin;%PATH% && `
    pacman --noconfirm -Syyuu && `
    pacman --noconfirm -S mingw-w64-x86_64-llvm mingw-w64-x86_64-rust && `
    del C:\msys64\mingw64\x86_64-w64-mingw32\lib\libpthread.dll.a && `
    mkdir C:\work

WORKDIR C:\work
ENV PATH=C:\msys64\usr\bin;C:\msys64\mingw64\bin
ENTRYPOINT cargo
# docker build -t francolin-build-win .
# docker run --rm --mount type=bind,source=$(Get-Location),target=C:\work francolin-build-win