
import scanpy as sc
import celltypist

from .cta_tool_interface import CTAToolInterface


class CelltypistInterface(CTAToolInterface):

    def annotate(expr_data, ref_data, labels: str, **kwargs):
        model = celltypist.train(ref_data, labels=labels)
        predictions = celltypist.annotate(
            expr_data, model=model, majority_voting=True)
        return predictions

    def convert(res, convert_to: str, **kwargs):
        if convert_to == 'labels':
            return res.predicted_labels.majority_voting
        else:
            raise ValueError('Invalid option for `covert_to`')
