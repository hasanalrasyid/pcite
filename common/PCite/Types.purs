module PCite.Types where

import Prelude

import Data.Maybe (Maybe)

type Model =
  { todos ∷ Array Todo
  , pending ∷ String
  , fresh ∷ Int
  , visibility ∷ Visibility
  }

data Visibility
  = All
  | Active
  | Completed

derive instance eqVisibility ∷ Eq Visibility

instance showVisibility ∷ Show Visibility where
  show = case _ of
    All → "All"
    Active → "Active"
    Completed → "Completed"

type Todo =
  { text ∷ String
  , completed ∷ Boolean
  , editing ∷ Boolean
  , id ∷ Int
  }

initialModel ∷ Model
initialModel =
  { todos:
    [ {text: "Buy Milk", completed: false, editing: false, id: 1}
    , {text: "else stuff", completed: true, editing: false, id: 2}
    ]
  , pending: ""
  , fresh: 3
  , visibility: All
  }

data Action
  = None
  | UpdatePending String
  | AddTodo
  | UpdateTodo Int String
  | ToggleTodo Int Boolean
  | EditingTodo Int Boolean
  | DeleteTodo Int
  | DeleteCompleted
  | ToggleAll Boolean
  | ChangeVisibility Visibility

newTodo ∷ String → Int → Todo
newTodo = { text: _, id: _, completed: false, editing: false }

modifyWhere ∷ forall f a. Functor f ⇒ (a → Boolean) → (a → a) → f a → f a
modifyWhere pred mod = map (\a → if pred a then mod a else a)

type Person = {id ∷ Int, name ∷ String, age ∷ Maybe Int}
