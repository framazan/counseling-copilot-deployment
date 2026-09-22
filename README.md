# Counseling Copilot Deployment

<img src="src/evals/logo.png" alt="Counseling Copilot banner, with Stanford and the Vandrevala Foundation's logos." width="480"/>

<!-- Badges -->
<a href="https://github.com/framazan/counseling-copilot-deployment"><img alt="GitHub Repo stars" src="https://img.shields.io/github/stars/framazan/counseling-copilot-deployment"></a>&nbsp;
<a><img alt="GitHub contributors" src="https://img.shields.io/badge/contributors-20-brightgreen"></a>&nbsp;
<a href="https://github.com/framazan/counseling-copilot-deployment/blob/main/LICENSE"><img alt="License" src="https://img.shields.io/github/license/framazan/counseling-copilot-deployment?color=blue"></a>

**Counseling Copilot Deployment** is the repository for the deployment and evaluation of a large language model copilot that drafts responses for counselors during live WhatsApp-based crisis conversations at a national mental health helpline in India.

The repository is organized into the following key components:

- `analysis/`: R and Python scripts, Jupyter notebooks, and documentation for causal modeling, evaluating counselor scores, generating figures, and analyzing deployment data.
- `prompts/`: Prompt templates used for client retention, client satisfaction, and message classification tasks.
- `scripts/`: Shell scripts for running end-to-end evaluation pipelines and executing LLM client requests.
- `src/`: Core Python modules for data export and evaluation.
  - `src/data_export/`: Tools to pull and export conversation tables from the database.
  - `src/evals/`: Scripts to build prompted datasets, extract completion contents, parse evaluation results, and estimate budget requirements.
  - `src/utils/`: Shared utilities and helper functions.

## Quick Start

<!--quick-start-begin-->

To set up the environment, clone the repository and install the required dependencies using `pip`:

```sh
git clone https://github.com/framazan/counseling-copilot-deployment.git
cd counseling-copilot-deployment
pip install -r requirements.txt
```

Set up your `.env` file with the required API keys (e.g., `OPENAI_API_KEY`) to run the LLM-based pipelines.

### Usage Guide

This repository provides tools for both exporting and analyzing data from the copilot deployment, as well as running new evaluations.

#### End-to-End Evaluation Pipeline
You can run the end-to-end retention and satisfaction pipeline which exports conversation logs, builds prompted datasets using templates, executes LLM evaluation calls, parses completion results, and calculates metrics.

```sh
bash scripts/run_deployment_retention_satisfaction_pipeline.sh
```

#### Running Analyses
Analytical scripts are located in the `analysis/` directory. For example, to generate figures or run causal modeling analysis:
```sh
Rscript analysis/r_scripts/causal_modeling_analysis.R
```

<!--quick-start-end-->

## Security and Privacy

For privacy and security reasons, real patient transcripts and data from the deployment are **not included** in this open-source release.

## Acknowledgements
We extend our deepest gratitude to the Vandrevala Foundation and their generous sponsors for providing the resources that made this analysis possible.

## Citation

If you use this software in your research, please cite our paper as below.

```bibtex
@article{
copilot2026,
title={Deployment and evaluation of an AI Copilot for mental health crisis counseling},
author={Akshay Swaminathan and Ivan Lopez and Sharang Phadke and Shaked Peleg Azzam and Akhilesh Laghate and Divyanjali Verma and Abhay John and Sharon Zhang and Shreya Shah and Jaimie Lim and William Wang and Ivy Pham and Filip Ramazan and Rebecca Hurwitz and Sebastian Garcia and Gloria Ye and Chastin Chung and Samuel Chuang and Ehsan Adeli and Nigam H. Shah},
journal={arXiv preprint arXiv:[Placeholder]},
year={2026},
url={[Placeholder for URL]}
}
```

## License

MIT License