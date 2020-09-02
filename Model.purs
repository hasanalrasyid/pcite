module Model where

import           Data.Aeson
import           Network.URI

import           Database.Persist.TH
import Data.Time
import Data.Maybe
import qualified Data.Text as T

import GHC.Generics

-- | Taken from http://www.yesodweb.com/book/persistent.

share [mkPersist sqlSettings, mkMigrate "migrateRefs"] [persistLowerCase|
Pegawai
     nama                T.Text  sqltype=varchar(100)
     nip                 T.Text  sqltype=varchar(50)
     kodeAbsen          Int     sqltype=int(20)
     unit                T.Text  sqltype=varchar(25)
     email               T.Text  sqltype=varchar(50)
     telp                T.Text  sqltype=varchar(100)
     password            T.Text  sqltype=varchar(50)
     tglLahir           Day     sqltype=date
     status              Int     sqltype=smallint(5)
     golongan            Int     sqltype=smallint(5)
     statusAktif        Int     sqltype=smallint(2)
     foto                T.Text  sqltype=varchar(200)
     tglData            UTCTime sqltype=datetime
     fpid                T.Text  sqltype=varchar(10)
     statSpkwt          Int     sqltype=int(1)
  deriving Show

ThesisType
  thesis T.Text
  deriving Show
Reference json
     author                T.Text
     address               T.Text     Maybe
     corporateAuthor       T.Text Maybe    sqltype=varchar(255)
     firstAuthor           T.Text     sqltype=varchar(100)
     authorCount           Int        sqltype=tinyint(3)
     title                 T.Text
     origTitle             T.Text Maybe
     publication            T.Text     Maybe                      sqltype=varchar(255)
     abbrevJournal          T.Text     Maybe                      sqltype=varchar(100)
     year                   Int        Maybe                      sqltype=smallint(6)
     volume                 T.Text     Maybe                      sqltype=varchar(50)
     volumeNumeric          Int        Maybe                      sqltype=smallint(5)
     issue                  T.Text     Maybe                      sqltype=varchar(50)
     pages                  T.Text     Maybe                      sqltype=varchar(50)
     firstPage              Int        Maybe                      sqltype=mediumint(8)
     keywords              T.Text Maybe
     abstract              T.Text Maybe
     edition               T.Text      Maybe                      sqltype=varchar(50)
     editor                T.Text  Maybe
     publisher             T.Text      Maybe                      sqltype=varchar(255)
     place                 T.Text      Maybe                      sqltype=varchar(100)
     medium                T.Text      Maybe                      sqltype=varchar(50)
     seriesEditor          T.Text Maybe
     seriesTitle           T.Text Maybe
     abbrevSeriesTitle     T.Text      Maybe                      sqltype=varchar(100)
     seriesVolume          T.Text      Maybe                      sqltype=varchar(50)
     seriesVolumeNumeric   Int         Maybe                      sqltype=smallint(5)
     seriesIssue           T.Text      Maybe                      sqltype=varchar(50)
     issn                  T.Text      Maybe                      sqltype=varchar(100)
     isbn                  T.Text      Maybe                      sqltype=varchar(100)
     language              T.Text      Maybe                      sqltype=varchar(100)
     summaryLanguage       T.Text      Maybe                      sqltype=varchar(100)
     area                  T.Text      Maybe                      sqltype=varchar(255)
     type                  T.Text      Maybe                      sqltype=varchar(100)
     thesis T.Text Maybe sqltype=enum('Bachelor_thesis','Honours_thesis','Master_thesis','Ph.D._thesis','Diploma_thesis','Doctoral_thesis','Habilitation_thesis')
     expedition            T.Text      Maybe                      sqltype=varchar(255)
     doi                   T.Text      Maybe                      sqltype=varchar(100)
     conference            T.Text      Maybe                      sqltype=varchar(255)
     url                   T.Text                      --           sqltype=varchar(191)
     callNumber            T.Text
     location              T.Text
     contributionId        T.Text     Maybe                       sqltype=varchar(100)
     onlinePublication     T.Text sqltype=enum('no','yes') default='no'
     onlineCitation        T.Text     Maybe                       sqltype=varchar(255)
     file                  T.Text     Maybe                       sqltype=varchar(255)
     notes                 T.Text Maybe
     serial                Int sqltype=mediumint(8)
     origRecord            Int Maybe               sqltype=mediumint(9)
     approved              T.Text sqltype=enum('no','yes') default='no'
     createdDate           Day Maybe
     createdTime           TimeOfDay  Maybe sqltype=time
     createdBy             T.Text   Maybe                         sqltype=varchar(100)
     modifiedDate          Day Maybe
     modifiedTime          TimeOfDay Maybe sqltype=time
     modifiedBy            T.Text   Maybe                         sqltype=varchar(100)
     version               Int sqltype=mediumint(8)  default=1
     Primary serial
     UniqueUrl url
     deriving Eq Show

RelationPR
     pId    PegawaiId
     refIds   [ReferenceId]
  deriving Show
|]

data SimpleRef = SimpleRef { refSerial      :: Int
                           , refAuthor      :: T.Text
                           , refTitle       :: T.Text
                           , refPublication :: T.Text
                           , refYear        :: Int
                           , refVolume      :: T.Text
                           , refPages       :: T.Text
                           , refPublisher   :: T.Text
                           } deriving (Generic,Show)

instance ToJSON SimpleRef
instance FromJSON SimpleRef

data Token = Token { token :: T.Text } deriving (Generic,Show)
instance FromJSON Token
instance ToJSON Token

data Abstract = Abstract { absSerial   :: Int
                         , absAbstract :: T.Text
                         } deriving (Generic,Show)

instance ToJSON Abstract
instance FromJSON Abstract

class FromReference a where
  fromReference :: Reference -> a

instance FromReference Abstract where
  fromReference r = Abstract  (referenceSerial r)
                              (fromMaybe "" $ referenceAbstract r)

instance FromReference SimpleRef where
  fromReference r = SimpleRef ( referenceSerial       r)
                              ( referenceAuthor       r)
                              ( referenceTitle        r)
                              ( fromMaybe ""    $ referencePublication  r)
                              ( fromMaybe 9999  $ referenceYear         r)
                              ( fromMaybe ""    $ referenceVolume       r)
                              ( fromMaybe ""    $ referencePages        r)
                              ( fromMaybe ""    $ referencePublisher    r)

data SearchMode = SAbstract | SAuthor | SKeywords | SOwner deriving (Generic,Ord,Eq)
instance ToJSON SearchMode
instance FromJSON SearchMode

data Search = Search       { searchMode :: SearchMode
                           , searchTerm :: T.Text
                           , searchPage :: Int
                           , searchOwnerID :: Int
                           } deriving (Generic)
instance ToJSON Search
instance FromJSON Search

data OwnerLRef = OwnerLRef { ownPID :: Int
                           , ownLRef :: [Int]
                           } deriving (Generic,Show)

instance ToJSON OwnerLRef
instance FromJSON OwnerLRef

data Person = Person { personName :: T.Text
                     , personId   :: Int
                     } deriving (Generic,Show)
instance ToJSON Person
instance FromJSON Person

data Model = Model
  { records :: Either T.Text [Reference]
  , currentURI :: URI
  , abstractOn :: Int
  , activePanel :: T.Text
  , previousPanel :: T.Text
  } deriving (Eq, Show)

initialModel :: URI -> Model
initialModel uri = Model
    { records = Left "Loading..."
    , currentURI = uri
    , abstractOn = 0
    , activePanel = "landing"
    , previousPanel = "landing"
    }

