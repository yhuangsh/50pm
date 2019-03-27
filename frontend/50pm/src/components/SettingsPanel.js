import React, { useState } from 'react';
import styled from 'styled-components';

import * as g from '../lib/g';

// Constants

// Stylings

const Frame = styled.div`
  margin: 1ex .5em;
  border: 1px solid black;

  display: flex;
  flex-flow: column;
  align-items: center;

`;

const Title = styled.h3`
  flex: 1;

  font-family: 'Fira Code', monospace;
  font-weight: 100;

  text-align: center;
`;

const OptionGroup = styled.div`
  width: 80%;
  flex: 1;
  
  display: flex;
  flex-flow: row nowrap;
  justify-items: space-around;
`;

const Option = styled.h4`
  flex: 1;

  color: ${props => props.enable ? 'black' : 'lightgrey'};
  font-family: 'Fira Code', monospace;
  font-weight: 100;

  text-align: center;

  :hover {
    cursor: pointer;
  }
`;

// Components

const DigitOptions = () => {
  const D1 = [true, false, false];
  const D2 = [false, true, false];
  const D3 = [false, false, true];

  let [opt, setOpt] = useState([g.options.digits]);
  console.log('DigitOption: config.options.digits=', g.options.digits);

  const onClick = (e, opt) => {
    setOpt(opt);
    g.options.digits = opt;
    console.log('onClick: config.options.digits=', g.options.digits);
  }

  return (
    <React.Fragment>
      <Title>...Digits...</Title>
      <OptionGroup>
        <Option enable={opt[0]} onClick={e => onClick(e, D1)}>1 digit</Option>
        <Option enable={opt[1]} onClick={e => onClick(e, D2)}>2 digits</Option>
        <Option enable={opt[2]} onClick={e => onClick(e, D3)}>3 digits</Option>
      </OptionGroup>
    </React.Fragment>
  );
}

const SettingsPanel = () => {
  let n1 = 10, op = 0, n2 = 20, n3 = 30;

  return (
    <Frame>
      <DigitOptions />
      <Title>...Unknowns...</Title>
      <OptionGroup>
        <Option>Left only</Option>
        <Option>Right only</Option>
        <Option>Left and Right</Option>
      </OptionGroup>
    </Frame>
  );
}

export default SettingsPanel;

// Utilities