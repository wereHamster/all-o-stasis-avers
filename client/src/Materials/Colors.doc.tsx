import * as React from "react";

import { markdown, ColorSpecimen, ColorPaletteSpecimen } from "catalog";

import * as C from "./Colors";

export default () => markdown`
# Brand Colors

${<ColorSpecimen value={C.primary} name="Primary" span={3} />}
${<ColorSpecimen value={C.lightPrimary} name="Light" span={2} />}
${<ColorSpecimen value={C.darkPrimary} name="Dark" span={1} />}

${<ColorSpecimen value={C.secondary} name="Secondary" span={3} />}
${<ColorSpecimen value={C.lightSecondary} name="Light" span={2} />}
${<ColorSpecimen value={C.darkSecondary} name="Dark" span={1} />}

# Monochrome

${<ColorPaletteSpecimen horizontal colors={C.monochromeColors} />}

# Grade colors

${<ColorPaletteSpecimen span={1} colors={C.yellowColors} />}
${<ColorPaletteSpecimen span={1} colors={C.greenColors} />}
${<ColorPaletteSpecimen span={1} colors={C.orangeColors} />}
${<ColorPaletteSpecimen span={1} colors={C.blueColors} />}
${<ColorPaletteSpecimen span={1} colors={C.redColors} />}
`;
