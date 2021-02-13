import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="pylernkarten",
    version="0.0.1",
    author="Danilo Mendonça Oliveira",
    author_email="danilomendoncaoliveira@gmail.com",
    description="A flashcard app for learning german",
    license="MIT",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/danilomo/PyLernkarten/",
    packages=setuptools.find_packages(),
    python_requires=">= 3",
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent"
    ],
    scripts=['bin/lernkarten'],
    entry_points = {
        'console_scripts': ['pylernkarten=pylernkarten.main:main'],
    }
)
