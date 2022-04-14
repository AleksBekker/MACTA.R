
from abc import ABC, abstractmethod


class CTAToolInterface(ABC):

    @abstractmethod
    def annotate(expr_data, ref_data, labels, **kwargs):
        pass

    @abstractmethod
    def convert(res, convert_to: str):
        pass
