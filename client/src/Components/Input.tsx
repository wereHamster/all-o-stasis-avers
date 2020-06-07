import styled from "styled-components";
import { useTypeface, copy16 } from "../Materials/Typefaces";
import { primary, darkPrimary, text } from "../Materials/Colors";

export const Input = styled.input`
  ${useTypeface(copy16)};
  color: ${text}DD;
  width: 100%;

  height: 40px;
  line-height: 40px;
  padding: 0 8px;

  outline: none;
  border: 1px solid ${primary};
  border-radius: 0;

  transition: all 0.16s;

  -webkit-appearance: none;

  &:focus {
    border-color: ${darkPrimary};
  }
`;
