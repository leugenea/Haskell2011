module Simply-Typed where

open import Data.List
open import Data.String
open import Level

infixr 5 _⇝_
data Type : Set where
  con : String → Type
  _⇝_ : Type → Type → Type

TList = List Type

infixr 4 _∈_

data _∈_ {a} {A : Set a} : A → List A → Set a where
  Z : ∀ {x xs} → x ∈ (x ∷ xs)
  S : ∀ {x y xs} → (n : x ∈ xs) → x ∈ (y ∷ xs)

infixl 5 _∙_
data Term (Γ : TList) : Type → Set where
  Var : ∀ {A} → A ∈ Γ → Term Γ A
  _∙_ : ∀ {A B} → Term Γ (A ⇝ B) → Term Γ A → Term Γ B
  Λ   : ∀ {A B} → Term (A ∷ Γ) B → Term Γ (A ⇝ B)

_⊆_ : ∀ {a} {A : Set a} → List A → List A → Set a
xs ⊆ ys = ∀ {x} → x ∈ xs → x ∈ ys

data _≡_ {l : Level} {A : Set l} : A → A → Set l where
  Refl : {a : A} → (a ≡ a)

⊆cong : ∀ {a} {A : Set a} {x y : A} {xs ys : List A} → x ≡ y → (xs ⊆ ys) → ((x ∷ xs) ⊆ (y ∷ ys))
⊆cong (Refl {x}) f Z = Z
⊆cong refl f (S n) = S (f n)

xs⊆x∷xs : ∀ {a} {A : Set a} {y : A} {xs : List A} → (xs ⊆ (y ∷ xs))
xs⊆x∷xs = S

wk : ∀ {Γ Δ A} → (Γ ⊆ Δ) → Term Γ A → Term Δ A
wk θ (Var y) = Var (θ y)
wk θ (y₁ ∙ y₂) = wk θ y₁ ∙ wk θ y₂
wk θ (Λ y) = Λ (wk (⊆cong Refl θ) y)

weaking : ∀ {Γ A B} → Term Γ B → Term (A ∷ Γ) B
weaking = wk xs⊆x∷xs

x∷y∷s⊆y∷x∷s : ∀ {a} {A : Set a} {x y : A} {s : List A} → ((x ∷ y ∷ s) ⊆ (y ∷ x ∷ s))
x∷y∷s⊆y∷x∷s Z = S Z
x∷y∷s⊆y∷x∷s (S Z) = Z
x∷y∷s⊆y∷x∷s (S (S n)) = S (S n)

exchange : ∀ {Γ A B C} → Term (A ∷ B ∷ Γ) C → Term (B ∷ A ∷ Γ) C
exchange = wk x∷y∷s⊆y∷x∷s