module Model where

data Cell = Empty | Number Int | Closed | Mine

data Position = Position Int Int

type Field = [[Cell]]
