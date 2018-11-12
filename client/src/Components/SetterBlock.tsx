import * as Avers from "avers";
import * as React from "react";
import styled from "styled-components";

import { accountAvatar } from "../Views/Account";
import { App, navigateToFn } from "../app";
import { grades, Boulder, publicProfile } from "../storage";

import { useTypeface, heading28, copy14 } from "../Materials/Typefaces";
import { text } from "../Materials/Colors";
import { GradeDistributionChart } from "./GradeDistributionChart";

export interface SetterCardProps {
  app: App;
  accountId: string;
}

export class SetterBlock extends React.Component<SetterCardProps> {
  render() {
    const { app, accountId } = this.props;

    const profile = Avers.staticValue(app.data.aversH, publicProfile(app.data.aversH, accountId)).get(undefined);

    const gradeDistribution = new Map<string, number>();
    grades.forEach(grade => {
      gradeDistribution.set(grade, (gradeDistribution.get(grade) || 0));
      app.data.activeBouldersCollection.ids.get<string[]>([]).forEach(boulderId => {
        const boulder = Avers.lookupContent<Boulder>(app.data.aversH, boulderId).get(undefined);
        if (boulder && boulder.grade === grade && boulder.setter.some(x => x === accountId)) {
          gradeDistribution.set(grade, (gradeDistribution.get(grade) || 0) + 1);
        }
      });
    });
    const sum = Array.from(gradeDistribution.values()).reduce((a, c) => a + c, 0);

    const boulderFrequencyDistribution = Array.from(gradeDistribution.entries()).map(([k, v]) => {
      return { grade: k, count: v };
    });

    return (
      <Root>
        <Top>
          <Avatar onClick={navigateToFn("/account/" + accountId)} src={accountAvatar(app.data.aversH, accountId)} />
          <div>
            <Name>{profile && profile.name !== "" ? profile.name : accountId.slice(0, 5)}</Name>
            <Tagline>Hat {sum} boulder an der wand</Tagline>
          </div>
        </Top>
        <Bottom>
          <GradeDistributionChart data={boulderFrequencyDistribution} />
        </Bottom>
      </Root>
    );
  }
}

const Root = styled.div`
  background: white;
  padding: 24px;
  box-shadow: 0 0 4px 0 rgba(0, 0, 0, 0.1);
  transition: box-shadow 0.16s;

  &:hover {
    box-shadow: 0 0 8px 0 rgba(0, 0, 0, 0.2);
  }
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

const Tagline = styled.div`
  ${useTypeface(copy14)};
  color: #222222bb;
`;
