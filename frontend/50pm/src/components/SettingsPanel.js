import React from 'react';
import styled from 'styled-components';

import * as defaults from '../lib/defaults';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;

  @media print {
    display: none;
  }
`;

const Title = styled.h4`
  font-family: 'Montserrat', sans-serif;
  font-weight: 100;
  text-align: center;
`;

const OptionGroup = styled.div`  
  display: flex;
  flex-flow: row nowrap;
  justify-items: space-around;
  align-items: baseline;
`;

const Option = styled.h5`
  flex: 1;

  color: ${props => props.enable ? 'black' : 'grey'};
  font-size: ${props => props.enable ? '110%' : '100%'};

  font-family: 'Dosis', cursive;
  font-weight: 100;

  text-align: center;

  :hover {
    cursor: pointer;
  }
`;

// Components

const PMTDOptions = (props) => {
  const O1 = [true, false];
  const O2 = [false, true];

  const opt = props.pmtdOpt;

  return (
    <React.Fragment>
      <Title>... Calculation ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickPmtd(e, O1)}>Plus & Minus</Option>
        <Option enable={opt[1]} onClick={e => props.onClickPmtd(e, O2)}>Times & Divides</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const DigitOptions = (props) => {
  const O1 = [true, false, false];
  const O2 = [false, true, false];
  const O3 = [false, false, true];

  const digitOptTexts = getDigitOptText(props.pmtdOpt);
  const opt = props.digitOpt;

  return (
    <React.Fragment>
      <Title>... Digits ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickDigit(e, O1)}>{digitOptTexts[0]}</Option>
        <Option enable={opt[1]} onClick={e => props.onClickDigit(e, O2)}>{digitOptTexts[1]}</Option>
        <Option enable={opt[2]} onClick={e => props.onClickDigit(e, O3)}>{digitOptTexts[2]}</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const UnknownOptions = (props) => {
  const O1 = [true, false, false];
  const O2 = [false, true, false];
  const O3 = [false, false, true];

  const opt = props.unknownOpt;

  return (
    <React.Fragment>
      <Title>... Unknowns on ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickUnknown(e, O1)}>Left side</Option>
        <Option enable={opt[1]} onClick={e => props.onClickUnknown(e, O2)}>Right side</Option>
        <Option enable={opt[2]} onClick={e => props.onClickUnknown(e, O3)}>Both sides</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const PageOptions = (props) => {
  const P1 = [true, false, false, false];
  const P2 = [false, true, false, false];
  const P3 = [false, false, true, false];
  const P4 = [false, false, false, true];

  const opt = props.pageOpt;

  return (
    <React.Fragment>
      <Title>... Pages ...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => props.onClickPage(e, P1)}>1 page</Option>
        <Option enable={opt[1]} onClick={e => props.onClickPage(e, P2)}>2 pages</Option>
        <Option enable={opt[2]} onClick={e => props.onClickPage(e, P3)}>4 pages</Option>
        <Option enable={opt[3]} onClick={e => props.onClickPage(e, P4)}>10 pages</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const SettingsPanel = (props) => {
  return (
    <Frame>
      <PMTDOptions {...props} />
      <DigitOptions {...props} /> 
      <UnknownOptions {...props} />
      <PageOptions {...props} />
    </Frame>
  );
}

export default SettingsPanel;

// Utilities

const getDigitOptText = (pmtdOpt) => {
  const [p1, p2] = pmtdOpt;

  if (p1) return ['1 digit', '2 digits', '3 digits'];
  if (p2) return ['1 digit', '2 digits x 1 digit', '2 digits x 2 digits'];

  console.log('getDigitOptText ERROR: bad pmtdOpt');
  return [false, false, false];
}