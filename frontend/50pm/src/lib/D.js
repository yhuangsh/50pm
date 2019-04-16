export const showHelp = false;

export const pmtdOpt = () => (JSON.parse(localStorage.getItem('pmtdOpt')) || [true, false]);
export const digitOpt = () => (JSON.parse(localStorage.getItem('digitOpt')) || [false, true, false]);
export const unknownOpt = () => (JSON.parse(localStorage.getItem('unknownOpt')) || [false, false, true]);
export const pageOpt = () => (JSON.parse(localStorage.getItem('pageOpt')) || [true, false, false, false]);

export const savePmtdOpt = (opt) => localStorage.setItem('pmtdOpt', JSON.stringify(opt));
export const saveDigitOpt = (opt) => localStorage.setItem('digitOpt', JSON.stringify(opt));
export const saveUnknownOpt = (opt) => localStorage.setItem('unknownOpt', JSON.stringify(opt));
export const savePageOpt = (opt) => localStorage.setItem('pageOpt', JSON.stringify(opt));