
export interface Typeface {
    fontFamily: string
    fontSize: string
    fontWeight: string
    lineHeight: string
}

export const useTypeface = (tf: Typeface): string => `
    font-family: ${tf.fontFamily};
    font-size: ${tf.fontSize};
    font-weight: ${tf.fontWeight};
    line-height: ${tf.lineHeight};
`


// ----------------------------------------------------------------------------
export const headingFontFamily = '-apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif'

export const h1: Typeface = {
    fontFamily: headingFontFamily,
    fontSize: '28px',
    fontWeight: 'normal',
    lineHeight: '1.4',
}


// ----------------------------------------------------------------------------
export const copyFontFamily = '-apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif'

export const copy16: Typeface = {
    fontFamily: copyFontFamily,
    fontSize: '16px',
    fontWeight: 'normal',
    lineHeight: '1.6',
}
