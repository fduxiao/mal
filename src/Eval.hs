module Eval where

import Control.Monad.IO.Class

newtype Eval env err a = Eval {runEval :: env -> IO (Either err a, env)}

instance Monad (Eval env err) where
    return a = Eval $ \env -> return (Right a, env)
    (Eval ma) >>= f = Eval $ \env -> do
        (result, env') <- ma env
        case result of
            Left e -> return (Left e, env')
            Right a -> runEval (f a) env'

instance MonadIO (Eval env err) where
    liftIO io = Eval $ \env -> do
        a <- io
        return (Right a, env)

instance Applicative (Eval env err) where
    pure = return
    f <*> a = f >>= (<$>a)

instance Functor (Eval env err) where
    fmap f a = a >>= (return . f)

getEnv :: Eval env err env
getEnv = Eval $ \env -> return (Right env, env)

putEnv :: env -> Eval env err ()
putEnv env = Eval $ \_ -> return (Right (), env)

modifyEnv :: (env -> env) -> Eval env err ()
modifyEnv f = getEnv >>= (putEnv . f)

throw :: err -> Eval env err a
throw err = Eval $ \env -> return (Left err, env)
