module DRcon.Version (
    versionStr,
    programName,
    versionInfo,
    licenseText
) where
import Text.Printf

#ifdef CABAL_VERSION
import Paths_darkplaces_rcon_util (version)
import Data.Version (showVersion)

versionStr = showVersion version
#else
versionStr = "dev"
#endif

versionStr :: String


programName :: String
programName = "drcon"


versionInfo :: String
versionInfo = printf "%s version %s\n" programName versionStr ++ licenseText


licenseText :: String
licenseText = unlines ["Copyright (C) Slava Bacherikov",
    "",
    "This program is free software; you can redistribute it and/or",
    "modify it under the terms of the GNU General Public License",
    "as published by the Free Software Foundation; either version 2",
    "of the License, or (at your option) any later version.",
    "",
    "This program is distributed in the hope that it will be useful,",
    "but WITHOUT ANY WARRANTY; without even the implied warranty of",
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
    "GNU General Public License for more details.",
    "",
    "You should have received a copy of the GNU General Public License",
    "along with this program; if not, write to the Free Software",
    "Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA."]
