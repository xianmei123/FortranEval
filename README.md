###### 代码说明：

大模型科学计算代码生成测试框架。需实现调用大模型根据数据集prompt生成代码的函数，即可自动测试该大模型生成代码的编译通过率，正确率，运行效率等指标。

###### 使用说明：

- 数据集:fortran_data/FortranEval.jsonl，每一个task包括id，prompt，正确标准代码，测试代码。
- generate_samples.py中的generate_one_completion需要改成获取模型生成的代码，运行generate_samples得到的数据可以在samples.jsonl中查看。
- 先运行generate_samples.py得到测试代码，再运行evaluate_functional_correctness.py得到测试结果，可以在samples.jsonl_results.jsonl查看更详细的测试结果。
- 指标：correct_pass@k:代码通过测试比例;compile_pass@k:代码通过编译比例;correct_compile_pass@k:通过编译的代码中通过测试的比例;passed_mean_time:通过测试的代码平均运行时间。