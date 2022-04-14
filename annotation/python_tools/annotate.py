
import os
import scanpy as sc

from tools.celltypist_interface import CelltypistInterface

tool_info_default = {
    'ref': {
        'celltypist': CelltypistInterface()
    },
    'marker': {

    }
}


def annotate(expr_data, ref_data, annot_type, result_type='labels',
             annot_tools='*', tool_info=tool_info_default, *args, **kwargs):
    pass
