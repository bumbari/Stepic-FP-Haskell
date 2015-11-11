import Control.Monad.Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]

pwds :: UsersTable
pwds = [("user", "123456"), ("x", "hi"), ("root", "123456")]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
	upl <- asks $ filter (("123456" ==) . snd)
	return $ map fst upl

main :: IO ()
main = mapM_ putStrLn (runReader usersWithBadPasswords pwds)