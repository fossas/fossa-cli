from setuptools import setup, find_packages

setup(
    name='sampleproject',
    version='1.3.0',
    description='A sample Python project',
    install_requires=['simple==1.0.0',
                      'gteq>=2.0.0', 'sameline==1.0.0',
                      'latest',
                      "with-double-quotes"
                      ],
)
