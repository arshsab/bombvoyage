(ns bombvoyage.specs
  (:require [bombvoyage.game.core :as game]
            [bombvoyage.chat :as chat]
            [bombvoyage.gameover :as game-over]))

(def specs
  {:game game/spec
   :chat chat/spec
   :game-over game-over/spec})

(def init-spec chat/spec)
