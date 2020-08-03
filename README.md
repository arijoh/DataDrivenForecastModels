# Summary

Hydrological systems benefit both economically and environmentally from high-quality models that can predict and prepare the system for an increase in runoff. Due to data-availability and advancement in machine learning, data-driven models, which are a  simplification of the conceptual models, have been gaining attention in recent years due to their speed and flexibility.

ARIMA is a statistical model that can be used for generating forecasts for hydrological systems. The order of the model is traditionally identified by manual inspection of the time series, but this manual inspection can both be tedious and time-consuming.

In this thesis, an automated model selection of ARIMA type models, using meta-optimization on two catchments in Copenhagen, is presented. 
For the model selection, the following things are investigated:

- How automated model selection can be carried out efficiently
- Comparison of models with/without precipitation as an input.
- How different optimization algorithms perform in the coefficient estimation (i.e. local/global-search).
- Comparision of models that are fitted using a single-, and multi-step forecasts.


Two different error measures will be used for model selection, Persistence Index skill-score (PI),  and accuracy in predicting a simplified version of ATS activation 

