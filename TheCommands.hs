module TheCommands where

import Darcs.Commands ( CommandControl(CommandData, GroupName) )
import Apply ( apply )
import Split ( split )

commandControlList :: [CommandControl]
commandControlList =
    [ GroupName "Changing files"
    , CommandData apply
    , GroupName "Changing patches"
    , CommandData split
    ]

