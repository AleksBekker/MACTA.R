
def save_labels_results(results, path: str, savefile_type: str):
    'Saves `labels` results in a file'


default_save_methods = {
    'labels': save_labels_results,
}


def save_results(results, path: str, results_type: str, savefile_type: str,
                 save_methods=default_save_methods):
    'Saves results into a file'

    save_methods[results_type](results, path, savefile_type)
