import React, { useState } from 'react';
import styled from 'styled-components';

import TopBar from '../components/TopBar';
import SettingsPanel from '../components/SettingsPanel';
import PrintPreview from '../components/PrintPreview';

import * as D from '../lib/D';
import { genEqusList } from '../lib/equ';

// Constants

// Stylings

const Frame = styled.div`
  width: 100%;
`;

// Components

const AppView = () => {
  const [pmtdOpt, setPmtdOpt] = useState(D.pmtdOpt());
  const [digitOpt, setDigitOpt] = useState(D.digitOpt());
  const [unknownOpt, setUnknownOpt] = useState(D.unknownOpt());
  const [pageOpt, setPageOpt] = useState(D.pageOpt());

  const [equs, setEqus] = useState(genEqusList(pmtdOpt, pages(pageOpt), 50, digitOpt, unknownOpt));

  const onClickPmtd = (e, newPmtdOpt) => {
    setPmtdOpt(newPmtdOpt);
    D.savePmtdOpt(newPmtdOpt);
    setEqus(genEqusList(newPmtdOpt, pages(pageOpt), 50, digitOpt, unknownOpt));
    //console.log('AppView.onClickPmtd: newPmtdOpt = ', newDPmtdpt);
  }

  const onClickDigit = (e, newDigitOpt) => {
    setDigitOpt(newDigitOpt);
    D.saveDigitOpt(newDigitOpt);
    setEqus(genEqusList(pmtdOpt, pages(pageOpt), 50, newDigitOpt, unknownOpt));
    //console.log('AppView.onClickDigit: newDigitOpt = ', newDigitOpt);
  }

  const onClickUnknown = (e, newUnknownOpt) => {
    setUnknownOpt(newUnknownOpt);
    D.saveUnknownOpt(newUnknownOpt);
    setEqus(genEqusList(pmtdOpt, pages(pageOpt), 50, digitOpt, newUnknownOpt));
    //console.log('AppView.onClickUnknown: newUnknownOpt =', newUnknownOpt);
  }

  const onClickPage = (e, newPageOpt) => {
    setPageOpt(newPageOpt);
    D.savePageOpt(newPageOpt);
    setEqus(genEqusList(pmtdOpt, pages(newPageOpt), 50, digitOpt, unknownOpt));
    //console.log('AppView.onClickPage: newPageOpt =', newPageOpt);
  }

  return (
    <Frame>
      <TopBar />
      <SettingsPanel 
        pmtdOpt={pmtdOpt} onClickPmtd={onClickPmtd}
        digitOpt={digitOpt} onClickDigit={onClickDigit}
        digitOptForTimes={digitOpt} onClickDigitForTimes={onClickDigit}
        unknownOpt={unknownOpt} onClickUnknown={onClickUnknown}
        pageOpt={pageOpt} onClickPage={onClickPage}
      />
      <PrintPreview equs={equs} />
    </Frame>
  );
}

export default AppView;

// Utilities

const pages = (pageOpt) => {
  const [p1, p2, p3, p4] = pageOpt;

  if (p1) return 1;
  if (p2) return 2;
  if (p3) return 4;
  if (p4) return 10;

  console.log('ERROR: bad pageOpt');
  return 0;
}