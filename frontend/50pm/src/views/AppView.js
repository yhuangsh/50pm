import React, { useState } from 'react';
import styled from 'styled-components';

import TopBar from '../components/TopBar';
import SettingsPanel from '../components/SettingsPanel';
import PrintPreview from '../components/PrintPreview';

import * as g from '../lib/g';
import { genEqus } from '../lib/equ';

// Constants

// Stylings

const Frame = styled.div`
  width: 100%;
`;

// Components

const AppView = () => {
  let [digitOpt, setDigitOpt] = useState(g.opts.digits);
  let [unknownOpt, setUnknownOpt] = useState(g.opts.unknowns);
  let [equs, setEqus] = useState([]);

  const onClickDigit = (e, newDigitOpt) => {
    setDigitOpt(newDigitOpt);
    setEqus([genEqus(50, newDigitOpt, unknownOpt)]);
    console.log('AppView.onClickDigit: newDigitOpt = ', newDigitOpt);
  }

  const onClickUnknown = (e, newUnknownOpt) => {
    setUnknownOpt(newUnknownOpt);
    setEqus([genEqus(50, digitOpt, newUnknownOpt)]);
    console.log('AppView.onClickUnknown: newUnknownOpt =', newUnknownOpt);
  }

  return (
    <Frame>
      <TopBar />
      <SettingsPanel 
        digitOpt={digitOpt} onClickDigit={onClickDigit}
        unknownOpt={unknownOpt} onClickUnknown={onClickUnknown}
      />
      <PrintPreview equs={equs} />
    </Frame>
  );
}

export default AppView;

// Utilities