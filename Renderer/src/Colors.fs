﻿namespace RendererFS


type Color = uint

type Colors =
    | Empty = 0x00000000u
    | AliceBlue = 0xFFF0F8FFu
    | AntiqueWhite = 0xFFFAEBD7u
    | Aqua = 0xFF00FFFFu
    | Aquamarine = 0xFF7FFFD4u
    | Azure = 0xFFF0FFFFu
    | Beige = 0xFFF5F5DCu
    | Bisque = 0xFFFFE4C4u
    | Black = 0xFF000000u
    | BlanchedAlmond = 0xFFFFEBCDu
    | Blue = 0xFF0000FFu
    | BlueViolet = 0xFF8A2BE2u
    | Brown = 0xFFA52A2Au
    | BurlyWood = 0xFFDEB887u
    | CadetBlue = 0xFF5F9EA0u
    | Chartreuse = 0xFF7FFF00u
    | Chocolate = 0xFFD2691Eu
    | Coral = 0xFFFF7F50u
    | CornflowerBlue = 0xFF6495EDu
    | Cornsilk = 0xFFFFF8DCu
    | Crimson = 0xFFDC143Cu
    | Cyan = 0xFF00FFFFu
    | DarkBlue = 0xFF00008Bu
    | DarkCyan = 0xFF008B8Bu
    | DarkGoldenrod = 0xFFB8860Bu
    | DarkGray = 0xFFA9A9A9u
    | DarkGreen = 0xFF006400u
    | DarkKhaki = 0xFFBDB76Bu
    | DarkMagenta = 0xFF8B008Bu
    | DarkOliveGreen = 0xFF556B2Fu
    | DarkOrange = 0xFFFF8C00u
    | DarkOrchid = 0xFF9932CCu
    | DarkRed = 0xFF8B0000u
    | DarkSalmon = 0xFFE9967Au
    | DarkSeaGreen = 0xFF8FBC8Bu
    | DarkSlateBlue = 0xFF483D8Bu
    | DarkSlateGray = 0xFF2F4F4Fu
    | DarkTurquoise = 0xFF00CED1u
    | DarkViolet = 0xFF9400D3u
    | DeepPink = 0xFFFF1493u
    | DeepSkyBlue = 0xFF00BFFFu
    | DimGray = 0xFF696969u
    | DodgerBlue = 0xFF1E90FFu
    | Firebrick = 0xFFB22222u
    | FloralWhite = 0xFFFFFAF0u
    | ForestGreen = 0xFF228B22u
    | Fuchsia = 0xFFFF00FFu
    | Gainsboro = 0xFFDCDCDCu
    | GhostWhite = 0xFFF8F8FFu
    | Gold = 0xFFFFD700u
    | Goldenrod = 0xFFDAA520u
    | Gray = 0xFF808080u
    | Green = 0xFF008000u
    | GreenYellow = 0xFFADFF2Fu
    | Honeydew = 0xFFF0FFF0u
    | HotPink = 0xFFFF69B4u
    | IndianRed = 0xFFCD5C5Cu
    | Indigo = 0xFF4B0082u
    | Ivory = 0xFFFFFFF0u
    | Khaki = 0xFFF0E68Cu
    | Lavender = 0xFFE6E6FAu
    | LavenderBlush = 0xFFFFF0F5u
    | LawnGreen = 0xFF7CFC00u
    | LemonChiffon = 0xFFFFFACDu
    | LightBlue = 0xFFADD8E6u
    | LightCoral = 0xFFF08080u
    | LightCyan = 0xFFE0FFFFu
    | LightGoldenrodYellow = 0xFFFAFAD2u
    | LightGray = 0xFFD3D3D3u
    | LightGreen = 0xFF90EE90u
    | LightPink = 0xFFFFB6C1u
    | LightSalmon = 0xFFFFA07Au
    | LightSeaGreen = 0xFF20B2AAu
    | LightSkyBlue = 0xFF87CEFAu
    | LightSlateGray = 0xFF778899u
    | LightSteelBlue = 0xFFB0C4DEu
    | LightYellow = 0xFFFFFFE0u
    | Lime = 0xFF00FF00u
    | LimeGreen = 0xFF32CD32u
    | Linen = 0xFFFAF0E6u
    | Magenta = 0xFFFF00FFu
    | Maroon = 0xFF800000u
    | MediumAquamarine = 0xFF66CDAAu
    | MediumBlue = 0xFF0000CDu
    | MediumOrchid = 0xFFBA55D3u
    | MediumPurple = 0xFF9370DBu
    | MediumSeaGreen = 0xFF3CB371u
    | MediumSlateBlue = 0xFF7B68EEu
    | MediumSpringGreen = 0xFF00FA9Au
    | MediumTurquoise = 0xFF48D1CCu
    | MediumVioletRed = 0xFFC71585u
    | MidnightBlue = 0xFF191970u
    | MintCream = 0xFFF5FFFAu
    | MistyRose = 0xFFFFE4E1u
    | Moccasin = 0xFFFFE4B5u
    | NavajoWhite = 0xFFFFDEADu
    | Navy = 0xFF000080u
    | OldLace = 0xFFFDF5E6u
    | Olive = 0xFF808000u
    | OliveDrab = 0xFF6B8E23u
    | Orange = 0xFFFFA500u
    | OrangeRed = 0xFFFF4500u
    | Orchid = 0xFFDA70D6u
    | PaleGoldenrod = 0xFFEEE8AAu
    | PaleGreen = 0xFF98FB98u
    | PaleTurquoise = 0xFFAFEEEEu
    | PaleVioletRed = 0xFFDB7093u
    | PapayaWhip = 0xFFFFEFD5u
    | PeachPuff = 0xFFFFDAB9u
    | Peru = 0xFFCD853Fu
    | Pink = 0xFFFFC0CBu
    | Plum = 0xFFDDA0DDu
    | PowderBlue = 0xFFB0E0E6u
    | Purple = 0xFF800080u
    | Red = 0xFFFF0000u
    | RosyBrown = 0xFFBC8F8Fu
    | RoyalBlue = 0xFF4169E1u
    | SaddleBrown = 0xFF8B4513u
    | Salmon = 0xFFFA8072u
    | SandyBrown = 0xFFF4A460u
    | SeaGreen = 0xFF2E8B57u
    | SeaShell = 0xFFFFF5EEu
    | Sienna = 0xFFA0522Du
    | Silver = 0xFFC0C0C0u
    | SkyBlue = 0xFF87CEEBu
    | SlateBlue = 0xFF6A5ACDu
    | SlateGray = 0xFF708090u
    | Snow = 0xFFFFFAFAu
    | SpringGreen = 0xFF00FF7Fu
    | SteelBlue = 0xFF4682B4u
    | Tan = 0xFFD2B48Cu
    | Teal = 0xFF008080u
    | Thistle = 0xFFD8BFD8u
    | Tomato = 0xFFFF6347u
    | Turquoise = 0xFF40E0D0u
    | Violet = 0xFFEE82EEu
    | Wheat = 0xFFF5DEB3u
    | White = 0xFFFFFFFFu
    | WhiteSmoke = 0xFFF5F5F5u
    | Yellow = 0xFFFFFF00u
    | YellowGreen = 0xFF9ACD32u
    | Transparent = 0x00FFFFFFu
