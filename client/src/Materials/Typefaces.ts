import { FontFamilyProperty, FontSizeProperty, FontWeightProperty, LineHeightProperty } from "csstype";

export interface Typeface {
  fontFamily: FontFamilyProperty;
  fontSize: FontSizeProperty<string | 0>;
  fontWeight: FontWeightProperty;
  lineHeight: LineHeightProperty<string | 0>;
}

export const useTypeface = (tf: Typeface): string => `
    font-family: ${tf.fontFamily};
    font-size: ${tf.fontSize};
    font-weight: ${tf.fontWeight};
    line-height: ${tf.lineHeight};
`;

// ----------------------------------------------------------------------------
export const headingFontFamily =
  '-apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif';

export const h1: Typeface = {
  fontFamily: headingFontFamily,
  fontSize: "28px",
  fontWeight: "normal",
  lineHeight: "1.4"
};

export const heading28: Typeface = {
  fontFamily: headingFontFamily,
  fontSize: "24px",
  fontWeight: "normal",
  lineHeight: "1.4"
};

export const heading24: Typeface = {
  fontFamily: headingFontFamily,
  fontSize: "24px",
  fontWeight: "normal",
  lineHeight: "1.4"
};

export const heading20: Typeface = {
  fontFamily: headingFontFamily,
  fontSize: "20px",
  fontWeight: "normal",
  lineHeight: "1.4"
};

export const heading18: Typeface = {
  fontFamily: headingFontFamily,
  fontSize: "18px",
  fontWeight: "normal",
  lineHeight: "1.4"
};

// ----------------------------------------------------------------------------
export const copyFontFamily =
  '-apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif';

export const copy16: Typeface = {
  fontFamily: copyFontFamily,
  fontSize: "16px",
  fontWeight: "normal",
  lineHeight: "1.6"
};

export const copy16Bold: Typeface = {
  fontFamily: copyFontFamily,
  fontSize: "16px",
  fontWeight: "bold",
  lineHeight: "1.6"
};

export const copy14: Typeface = {
  fontFamily: copyFontFamily,
  fontSize: "14px",
  fontWeight: "normal",
  lineHeight: "1.6"
};

export const copy14Bold: Typeface = {
  fontFamily: copyFontFamily,
  fontSize: "14px",
  fontWeight: "bold",
  lineHeight: "1.6"
};
