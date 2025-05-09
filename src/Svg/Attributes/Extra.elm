module Svg.Attributes.Extra exposing (height, width, x, y)

import Quantity exposing (Quantity, Unitless)
import Svg
import Svg.Attributes


x : Quantity Float Unitless -> Svg.Attribute msg
x q =
    Svg.Attributes.x (String.fromFloat (Quantity.toFloat q))


y : Quantity Float Unitless -> Svg.Attribute msg
y q =
    Svg.Attributes.y (String.fromFloat (Quantity.toFloat q))


width : Quantity Float Unitless -> Svg.Attribute msg
width q =
    Svg.Attributes.width (String.fromFloat (Quantity.toFloat q))


height : Quantity Float Unitless -> Svg.Attribute msg
height q =
    Svg.Attributes.height (String.fromFloat (Quantity.toFloat q))
