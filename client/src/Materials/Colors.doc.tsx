import * as React from 'react'

import {markdown, ColorPaletteSpecimen} from 'catalog'

import * as C from './Colors'

export default () => markdown`
# Monochrome

${
<ColorPaletteSpecimen horizontal colors={C.monochromeColors} />
}

# Grade colors

${
<ColorPaletteSpecimen span={1} colors={C.yellowColors} />
}
${
<ColorPaletteSpecimen span={1} colors={C.greenColors} />
}
${
<ColorPaletteSpecimen span={1} colors={C.orangeColors} />
}
${
<ColorPaletteSpecimen span={1} colors={C.blueColors} />
}
${
<ColorPaletteSpecimen span={1} colors={C.redColors} />
}
`
