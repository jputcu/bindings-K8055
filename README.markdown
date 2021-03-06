Description
-----------
This library allows you to control the Velleman K8055, a usb experiment interface board.

The board has:

 * 5 digital inputs
 * 2 analog inputs
 * 8 digital outputs
 * 2 analog outputs

How is works
------------
Velleman provides a `K8055D.dll`, so this is a **Windows-only solution**, to interact with the board.
A FFI interface has been written to interact with the dll functions.
For this I had to create a definition file `K8055D.def` and run `dlltool -k -d K8055D.def -l libK8055D.a`.

Some Background
---------------
My first attempt at interfacing this board was using `bindings-libusb`. After getting is to work
on Linux I was unable to compile it on Windows. Because my solution should work on Windows
I chose to interface the existing dll.

I haven't given up on the my first solution, it's still the cleanest way to go, having no
K8055 specific, native library dependencies and being platform-independent.

Install
-------
As mentioned earlier, we need the `K8055D.dll`. This can be obtained by downloading the official
[Velleman K8055 SDK]. Extract it and put the dll, `DLL_v4.0.0.0/K8055D.dll`, somewhere in PATH,
e.g. `C:\Windows\System32`.

Then all you need to do is install the cabal package:

    cabal install Bindings-K8055

Example
-------
Press a button, digital in, and try the following ghci code:

    > import Bindings.K8055
    > withDevice Card1 (readAllDigital >>= print)
    0

Or compiled. Watch out, you need the `libK8055D.a` interface library, which was packaged in this cabal
package and was probably installed in `%APPDATA%\cabal\bindings-K8055-0.1.2`. By putting it in the
subdirectory `ghc-<version>` it should be found by the compiler.

    import Bindings.K8055

    main :: IO ()
    main =
      withDevice Card1 (readAllDigital >>= print)


[Velleman K8055 SDK]: http://www.velleman.eu/downloads/files/downloads/k8055_sdk_version4.zip