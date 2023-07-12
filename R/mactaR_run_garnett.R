library(garnett)
library(Seurat)
library(org.Hs.eg.db)

#Convert the markerLists list objects I use to garnett compatible ones. Uses a for loop, shame on me.
# @param markerList - List object where each element is a character vector of markers expressed by the cell type denoted by the elements name. 
# Alternatively, list object of list objects, one for each cell type, where each sub-list has 2 character vectors named 'Positive' and 'Negative'
# Make sure to have the same number of elements in each sublist. Add NULLS if you need to.
#TODO: Add subtype specification allowed by Garnett.
markerlist_to_garnettfile <- function(markerList){
  initstring = ""
  if(all(lengths(markerList)) == 2){
    for(i in seq(1,length(markerList))){
      initstring = c(initstring,paste(">",names(markerList)[[i]],sep=""))
      #initstring = c(initstring,"\n")
      initstring = c(initstring,paste("expressed:", paste(markerList[[i]][["Positive"]],sep="",collapse=", ")))
      #initstring = c(initstring,"\n")
      initstring = c(initstring,paste("not expressed:", paste(markerList[[i]][["Negative"]],sep="",collapse=", ")))
      #initstring = c(initstring,"\n")
    }
    
  }else{
    for(i in seq(1,length(markerList))){
      initstring = c(initstring,paste(">",names(markerList)[[i]],sep=""))
      #initstring = c(initstring,"\n")
      initstring = c(initstring,paste("expressed:", paste(markerList[[i]],sep="",collapse=", ")))
      #initstring = c(initstring,"\n")
    }
  }
  fileConn<-file("garnett_markertable.txt")
  writeLines(initstring, fileConn)
  close(fileConn)
}

# Wrapper for the Garnett function. Check that function's documentation (garnett::train_cell_classifier)
#Note, you can add the argument garnett_classifier_path
garnett_train_classifier <- function(cds,
                                     marker_file_path, 
                                     db,
                                     ...){
  args = list(
    ...,
    cds_gene_id_type = "SYMBOL",
    num_unknown=50, 
    marker_file_gene_id_type = "SYMBOL") 
  if("garnett_classifier_path" %in% names(args)){
    if(file.exists(args$garnett_classifier_path)){
      classifier = readRDS(args$garnett_classifier_path)
    }else{
      classifier <- train_cell_classifier(cds = cds,
                                          marker_file = marker_file_path,
                                          db=db,
                                          cds_gene_id_type = args$cds_gene_id_type,
                                          marker_file_gene_id_type = args$marker_file_gene_id_type,
                                          num_unknown = args$num_unknown
      )
      saveRDS(classifier,args$garnett_classifier_path)
    }
    }else{
      classifier <- train_cell_classifier(cds = cds,
                                          marker_file = markier_file_path,
                                          db=db,
                                          cds_gene_id_type = args$cds_gene_id_type,
                                          marker_file_gene_id_type = args$marker_file_gene_id_type,
                                          num_unknown = args$num_unknown
      )
    }
    
  return(classifier)
}

# Convert a seurat object to the CDS file needed by Garnett. Currently works with Seurat 4, need to add check for Seurat v5 (basically LayerData vs GetAssayData)
# @param seur_obj, your seurat object
# @param assay, which assay you'll grab
# @param slot, which slot of your assay you'll grab
seurat_to_garnettcds.query <- function(seur_obj,...){
  args <- list(
    ...,
    assay="RNA",
    slot="counts"
  )
  
  phenoData <- new("AnnotatedDataFrame", data = seur_obj@meta.data)
  featureData <- new("AnnotatedDataFrame", data = seur_obj[[args$assay]][[]])
  expr_mat <- GetAssayData(seur_obj,assay=args$assay,slot=args$slot)
  celldataset <- newCellDataSet(expr_mat,
                                phenoData = phenoData,
                                featureData = featureData)
  celldataset <- estimateSizeFactors(celldataset)
  return(celldataset)
}

seurat_to_garnettcds.reference <- function(seur_obj,markerList,...){
  args <- list(
    ...,
    assay="RNA",
    slot="counts",
    cds_gene_id_type = "SYMBOL",
    num_unknown=50,
    marker_file_gene_id_type = "SYMBOL",
    db = org.Hs.eg.db
    
  )
  
  phenoData <- new("AnnotatedDataFrame", data = seur_obj@meta.data)
  featureData <- new("AnnotatedDataFrame", data = seur_obj[[args$assay]][[]])
  expr_mat <- GetAssayData(seur_obj,assay=args$assay,slot=args$slot)
  celldataset <- newCellDataSet(expr_mat,
                                phenoData = phenoData,
                                featureData = featureData)
  celldataset <- estimateSizeFactors(celldataset)
  markerlist_to_garnettfile(markerList)
  classifier <- garnett_train_classifier(celldataset,
                                         "garnett_markertable.txt", 
                                         args$db,
                                         cds_gene_id_type = args$cds_gene_id_type,
                                         marker_file_gene_id_type = args$marker_file_gene_id_type,
                                         num_unknown = args$num_unknown,
                                         ...
  )
  return(classifier)
}

# Wrapper for the Garnett function. Check that functions documentation (garnett::check_markers)
garnett_markercheck <- function(cds, marker_file_path, db, cds_gene_id_type="SYMBOL",marker_file_gene_id_type = "SYMBOL"){
  marker_check <- check_markers(cds, marker_file_path,
                                db=db,
                                cds_gene_id_type = cds_gene_id_type,
                                marker_file_gene_id_type = marker_file_gene_id_type)
  
  p = plot_markers(marker_check)
  return(p)
}

make_predictions.custom <- function (cds, classifier, curr_node, rank_prob_ratio, cores = 1, 
                                     s) 
{
  cvfit <- igraph::V(classifier@classification_tree)[curr_node]$model[[1]]
  predictions <- tryCatch({
    if (is.null(cvfit)) {
      child_cell_types <- igraph::V(classifier@classification_tree)[suppressWarnings(outnei(curr_node))]$name
      predictions <- matrix(FALSE, nrow = nrow(pData(cds)), 
                            ncol = length(child_cell_types), dimnames = list(row.names(pData(cds)), 
                                                                             child_cell_types))
      predictions <- split(predictions, rep(1:ncol(predictions), 
                                            each = nrow(predictions)))
      names(predictions) <- child_cell_types
      predictions
    }
    else {
      candidate_model_genes <- cvfit$glmnet.fit$beta[[1]]@Dimnames[[1]]
      good_genes <- intersect(row.names(exprs(cds)), candidate_model_genes)
      if (length(good_genes) == 0) 
        stop(paste("None of the model genes are in", 
                   "your CDS object. Did you", "specify the correct", 
                   "cds_gene_id_type and the", "correct db?"))
      x <- Matrix::t(exprs(cds[intersect(row.names(exprs(cds)), 
                                         candidate_model_genes), ]))
      extra <- as(matrix(0, nrow = nrow(x), ncol = length(setdiff(candidate_model_genes, 
                                                                  colnames(x)))), "sparseMatrix")
      row.names(extra) <- row.names(x)
      colnames(extra) <- setdiff(candidate_model_genes, 
                                 colnames(x))
      x <- cbind(x, extra)
      x <- x[, candidate_model_genes]
      nonz <- Matrix::rowSums(do.call(cbind, glmnet:::coef.glmnet(cvfit, 
                                                                  s = "lambda.min")))
      nonz <- nonz[2:length(nonz)]
      nonz <- names(nonz[nonz != 0])
      if (sum(!nonz %in% row.names(exprs(cds))) > 0) {
        warning(paste("The following genes used in the classifier are not", 
                      "present in the input CDS. Interpret with caution.", 
                      nonz[!nonz %in% row.names(exprs(cds))]))
      }
      temp <- stats::predict(cvfit, newx = x, s = s, type = "response")
      temp[is.nan(temp)] <- 0
      prediction_probs <- as.matrix(as.data.frame(temp))
      prediction_probs <- prediction_probs/Biobase::rowMax(prediction_probs)
      prediction_probs[is.nan(prediction_probs)] <- 0
      saveRDS(prediction_probs,"garnett_predictions.RDS")
      prediction_probs <- apply(prediction_probs, 1, function(x) {
        m <- names(which.max(x))
        s <- sort(x, decreasing = T)
        c(cell_type = m, odds_ratio = s[1]/s[2])
      })
      prediction_probs <- as.data.frame(t(prediction_probs))
      prediction_probs$cell_name <- row.names(prediction_probs)
      names(prediction_probs) <- c("cell_type", "odds_ratio", 
                                   "cell_name")
      prediction_probs$odds_ratio <- as.numeric(as.character(prediction_probs$odds_ratio))
      assignments <- prediction_probs[prediction_probs$odds_ratio > 
                                        rank_prob_ratio, ]
      random_guess_thresh <- 1/length(cvfit$glmnet.fit$beta)
      assignments <- assignments[assignments$odds_ratio > 
                                   random_guess_thresh, ]
      not_assigned <- row.names(pData(cds))[!row.names(pData(cds)) %in% 
                                              assignments$cell_name]
      if (length(not_assigned) > 0) {
        assignments <- rbind(assignments, data.frame(cell_name = not_assigned, 
                                                     cell_type = NA, odds_ratio = NA))
      }
      assignments$cell_type <- stringr::str_replace_all(assignments$cell_type, 
                                                        "\\.1", "")
      predictions <- reshape2::dcast(assignments, cell_name ~ 
                                       cell_type, value.var = "odds_ratio")
      predictions <- predictions[!is.na(predictions$cell_name), 
      ]
      row.names(predictions) <- predictions$cell_name
      if (ncol(predictions) > 2) {
        predictions <- predictions[, setdiff(colnames(predictions), 
                                             "NA")]
        predictions <- predictions[, -1, drop = FALSE]
        predictions <- predictions[rownames(pData(cds)), 
                                   , drop = FALSE]
        predictions <- as.matrix(predictions)
        predictions[is.na(predictions)] <- FALSE
        predictions[predictions != 0] <- TRUE
        cell_type_names <- colnames(predictions)
        predictions <- split(predictions, rep(1:ncol(predictions), 
                                              each = nrow(predictions)))
        names(predictions) <- cell_type_names
      }
      else {
        cell_type_names <- names(cvfit$glmnet.fit$beta)
        one_type <- names(predictions)[2]
        if (one_type == "NA") {
          names(predictions)[2] <- "Unknown"
          one_type <- "Unknown"
        }
        predictions <- matrix(FALSE, nrow = nrow(pData(cds)), 
                              ncol = length(cell_type_names), dimnames = list(row.names(pData(cds)), 
                                                                              cell_type_names))
        predictions[, one_type] <- TRUE
        predictions <- split(predictions, rep(1:ncol(predictions), 
                                              each = nrow(predictions)))
        names(predictions) <- cell_type_names
      }
      predictions
    }
  }, error = function(e) {
    if (e$message == paste("None of the model genes are in your CDS object.", 
                           "Did you specify the correct cds_gene_id_type and", 
                           "the correct db?")) 
      stop(e)
    print(e)
    cell_type_names <- names(cvfit$glmnet.fit$beta)
    predictions <- matrix(FALSE, nrow = nrow(pData(cds)), 
                          ncol = length(cell_type_names), dimnames = list(row.names(pData(cds)), 
                                                                          cell_type_names))
    predictions <- split(predictions, rep(1:ncol(predictions), 
                                          each = nrow(predictions)))
    names(predictions) <- cell_type_names
    predictions
  })
  for (i in 1:length(predictions)) {
    p <- as(as(predictions[[i]], "sparseVector"), "sparseMatrix")
    row.names(p) <- row.names(pData(cds))
    predictions[[i]] <- p
  }
  return(predictions)
}
assignInNamespace("make_predictions", make_predictions.custom, ns = "garnett")
# Wrapper for the Garnett function. Check that function's documentation (granett::classify_cells). 
garnett_annotate <- function(cds, classifier,...){
  args <- list(
    ...,
    db = org.Hs.eg.db,
    cluster_extend= T,
    cds_gene_id_type = "SYMBOL"
  )
  #This one was actually not as simple, this is code extracted from their package to get the labels.df format.
  cds <- classify_cells(cds, classifier,
                        db = args$db,
                        cluster_extend = args$cluster_extend,
                        cds_gene_id_type = args$cds_gene_id_type)
  if(args$cluster_extend){
    labels.df = readRDS("garnett_predictions.RDS")
    labels = unlist(pData(cds)[c("cluster_ext_type")])
  }else{
    labels = unlist(pData(cds)[c("cluster_ext_type")])
  }
  return(labels)
}
# TODO: I should re-list the params here. Add timers.


#TODO: Ask Aleks about the CTAInterface.


