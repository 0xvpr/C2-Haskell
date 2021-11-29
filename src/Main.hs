module Main where 
    import Network.Socket
    import Network.Socket
    import Network.Socket.ByteString
    import qualified Data.ByteString.Char8 as C

    main::IO()
    main = do
        sock<-socket AF_INET Stream 0
        bind sock (SockAddrInet 8001 0)
        listen sock 2
        mainloop sock


    mainloop::Socket->IO()
    mainloop sock = do
        conn<-accept sock
        runConn conn
        mainloop sock


    runConn::(Socket,SockAddr)->IO()
    runConn (sock,_)=do
        Network.Socket.ByteString.send sock (C.pack "hello")
        close sock
