import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { accountGravatarUrl } from "../Views/Account";
import { App, navigateToFn } from "../app";
import { Account, setterMonthlyStats } from "../storage";

import { useTypeface, heading28, copy14 } from "../Materials/Typefaces";
import { gradeBackgroundColor, text } from "../Materials/Colors";

export interface SetterCardProps {
  app: App;
  accountId: string;
  account: undefined | Account;
}

export const SetterBlock = ({ app, accountId, account }: SetterCardProps) => (
  <Root>
    <Top>
      <Avatar
        onClick={navigateToFn("/account/" + accountId)}
        src={account ? accountGravatarUrl(account.email) : placeholderImageSrc}
      />
      <div>
        <Name>{account && account.name !== "" ? account.name : accountId.slice(0, 5)}</Name>
        {/* <Tagline>Bewertet eigene Boulder gelegentlich zu leicht</Tagline> */}
      </div>
    </Top>
    <Bottom>
      {Avers.staticValue(
        app.data.aversH,
        setterMonthlyStats(app.data.aversH, accountId, new Date().getUTCFullYear(), new Date().getUTCMonth() + 1)
      )
        .fmap(sms => {
          const max = Math.max(...Object.keys(sms).map(grade => sms[grade]));
          const sum = Object.keys(sms)
            .map(grade => sms[grade])
            .reduce((a, n) => a + n, 0);

          return (
            <BoulderFrequencyDistribution>
              {Object.keys(sms).map(grade => {
                return (
                  <div
                    key={grade}
                    style={{
                      position: "relative",
                      width: 60,
                      height: (40 * sms[grade]) / max,
                      background: gradeBackgroundColor(grade.toLowerCase())
                    }}
                  >
                    <Percentage>{Math.round((100 * sms[grade]) / sum)}%</Percentage>
                  </div>
                );
              })}
            </BoulderFrequencyDistribution>
          );
        })
        .get(<BoulderFrequencyDistribution />)}
    </Bottom>
  </Root>
);

const placeholderImageSrc =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAAAAACoWZBhAAAAF0lEQVQI12P4BAI/QICBFCaYBPNJYQIAkUZftTbC4sIAAAAASUVORK5CYII=";

const Root = styled.div`
  margin: 40px 0;
`;

const Top = styled.div`
  display: flex;
  flex-direction: row;
`;

const Bottom = styled.div`
  margin-left: 74px;
`;

const Avatar = styled.img`
  height: 64px;
  width: 64px;
  border: 1px solid #999;
  border-radius: 50%;
  margin-right: 10px;
`;

const Name = styled.div`
  ${useTypeface(heading28)};
  color: ${text};
`;

/*
const Tagline = styled.div`
  ${useTypeface(copy14)};
  color: #222222bb;
`;
*/

const BoulderFrequencyDistribution = styled.div`
  display: flex;
  align-items: flex-end;
  height: 60px;
  margin-bottom: 20px;
`;

const Percentage = styled.div`
  ${useTypeface(copy14)};
  color: #222222bb;
  text-align: center;
  position: absolute;
  left: 0;
  right: 0;
  bottom: 0;
  margin-bottom: -22px;
`;
