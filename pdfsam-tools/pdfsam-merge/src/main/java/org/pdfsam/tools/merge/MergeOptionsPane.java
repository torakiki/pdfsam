/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.tools.merge;

import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.scale.PageNormalizationPolicy;
import org.sejda.model.toc.ToCPolicy;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Panel for the Merge options
 *
 * @author Andrea Vacondio
 */
class MergeOptionsPane extends VBox
        implements TaskParametersBuildStep<MergeParametersBuilder>, RestorableView, ResettableView {
    private final ComboBox<ComboItem<AcroFormPolicy>> acroForms = new ComboBox<>();
    private final CheckBox blankIfOdd;
    private final CheckBox footer;
    private final ComboBox<ComboItem<PageNormalizationPolicy>> pageNormalization = new ComboBox<>();
    private final ComboBox<ComboItem<OutlinePolicy>> outline = new ComboBox<>();
    private final ComboBox<ComboItem<ToCPolicy>> toc = new ComboBox<>();

    MergeOptionsPane() {
        super(5);
        var blankIfOddHelp = i18n().tr(
                "Adds a blank page after each merged document if the document has an odd number of pages");
        blankIfOdd = new CheckBox(i18n().tr("Add a blank page if page number is odd"));
        blankIfOdd.setGraphic(helpIcon(blankIfOddHelp));
        blankIfOdd.setAccessibleHelp(blankIfOddHelp);
        blankIfOdd.getStyleClass().addAll(Style.WITH_HELP.css());
        blankIfOdd.setId("blankIfOddCheck");

        var footerHelp = i18n().tr("Adds a page footer with the name of the file the page belonged to.");
        footer = new CheckBox(i18n().tr("Add a footer"));
        footer.setGraphic(helpIcon(footerHelp));
        footer.setAccessibleHelp(footerHelp);
        footer.getStyleClass().addAll(Style.WITH_HELP.css());
        footer.setId("footerCheck");

        GridPane options = new GridPane();

        pageNormalization.getItems().add(new ComboItem<>(PageNormalizationPolicy.NONE, i18n().tr("None")));
        pageNormalization.getItems()
                .add(new ComboItem<>(PageNormalizationPolicy.SAME_WIDTH, i18n().tr("Same width as first page")));
        pageNormalization.getItems().add(new ComboItem<>(PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED,
                i18n().tr("Same width as first page (based on page orientation)")));
        pageNormalization.setId("normalizeCheck");
        pageNormalization.setAccessibleText(i18n().tr("Pages normalization"));
        var pageNormalizationHelp = i18n().tr(
                "Set whether pages should be resized to all have the same width as the first one");
        pageNormalization.setAccessibleHelp(pageNormalizationHelp);
        var pageNormalizationLabel = new Label(i18n().tr("Pages normalization") + ":");
        pageNormalizationLabel.setLabelFor(pageNormalization);
        options.add(pageNormalizationLabel, 0, 0);
        pageNormalization.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(pageNormalization, 1, 0);
        options.add(helpIcon(pageNormalizationHelp), 2, 0);

        acroForms.getItems().add(new ComboItem<>(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS,
                i18n().tr("Merge renaming existing fields")));
        acroForms.getItems().add(new ComboItem<>(AcroFormPolicy.MERGE, i18n().tr("Merge fields")));
        acroForms.getItems().add(new ComboItem<>(AcroFormPolicy.FLATTEN, i18n().tr("Flatten")));
        acroForms.getItems().add(new ComboItem<>(AcroFormPolicy.DISCARD, i18n().tr("Discard forms")));
        acroForms.setId("acroFormsCombo");
        acroForms.setAccessibleText(i18n().tr("Interactive forms (AcroForms)"));
        var acroFormsHelp = i18n().tr("What to do in case one or more input documents contain Acro Forms");
        acroForms.setAccessibleHelp(acroFormsHelp);
        var acroFormsLabel = new Label(i18n().tr("Interactive forms (AcroForms)") + ":");
        acroFormsLabel.setLabelFor(acroForms);
        options.add(acroFormsLabel, 0, 1);
        acroForms.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(acroForms, 1, 1);
        options.add(helpIcon(acroFormsHelp), 2, 1);

        outline.getItems().add(new ComboItem<>(OutlinePolicy.RETAIN, i18n().tr("Retain bookmarks")));
        outline.getItems().add(new ComboItem<>(OutlinePolicy.DISCARD, i18n().tr("Discard bookmarks")));
        outline.getItems().add(new ComboItem<>(OutlinePolicy.ONE_ENTRY_EACH_DOC,
                i18n().tr("Create one entry for each merged document")));
        outline.getItems().add(new ComboItem<>(OutlinePolicy.RETAIN_AS_ONE_ENTRY,
                i18n().tr("Retain bookmarks as one entry for each merged document")));
        outline.setId("outlineCombo");
        outline.setAccessibleText(i18n().tr("Bookmarks handling"));
        var outlineHelp = i18n().tr("What to do in case one or more input documents contain bookmarks");
        outline.setAccessibleHelp(outlineHelp);
        var outlineLabel = new Label(i18n().tr("Bookmarks handling") + ":");
        outlineLabel.setLabelFor(outline);
        options.add(outlineLabel, 0, 2);
        outline.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(outline, 1, 2);
        options.add(helpIcon(outlineHelp), 2, 2);

        toc.getItems().add(new ComboItem<>(ToCPolicy.NONE, i18n().tr("Don't generate")));
        toc.getItems().add(new ComboItem<>(ToCPolicy.FILE_NAMES, i18n().tr("Generate from file names")));
        toc.getItems().add(new ComboItem<>(ToCPolicy.DOC_TITLES, i18n().tr("Generate from documents titles")));
        toc.setId("tocCombo");
        toc.setAccessibleText(i18n().tr("Table of contents"));
        var tocHelp = i18n().tr("Set if a table of contents should be added to the generated PDF document");
        toc.setAccessibleHelp(tocHelp);
        var tocLabel = new Label(i18n().tr("Table of contents") + ":");
        tocLabel.setLabelFor(toc);
        options.add(tocLabel, 0, 3);
        toc.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(toc, 1, 3);
        options.add(helpIcon(tocHelp), 2, 3);
        options.getStyleClass().addAll(Style.GRID.css());

        getStyleClass().addAll(Style.CONTAINER.css());
        resetView();
        getChildren().addAll(blankIfOdd, footer, options);
    }

    @Override
    public void resetView() {
        blankIfOdd.setSelected(false);
        footer.setSelected(false);
        pageNormalization.getSelectionModel().selectFirst();
        acroForms.getSelectionModel().selectFirst();
        outline.getSelectionModel().selectFirst();
        toc.getSelectionModel().selectFirst();
    }

    @Override
    public void apply(MergeParametersBuilder builder, Consumer<String> onError) {
        builder.outlinePolicy(outline.getSelectionModel().getSelectedItem().key());
        builder.acroFormsPolicy(acroForms.getSelectionModel().getSelectedItem().key());
        builder.tocPolicy(toc.getSelectionModel().getSelectedItem().key());
        builder.blankPageIfOdd(blankIfOdd.isSelected());
        builder.footer(footer.isSelected());
        builder.pageNormalizationPolicy(pageNormalization.getSelectionModel().getSelectedItem().key());
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("outline",
                Optional.ofNullable(outline.getSelectionModel().getSelectedItem()).map(i -> i.key().toString())
                        .orElse(EMPTY));
        data.put("acroForms",
                Optional.ofNullable(acroForms.getSelectionModel().getSelectedItem()).map(i -> i.key().toString())
                        .orElse(EMPTY));
        data.put("toc", Optional.ofNullable(toc.getSelectionModel().getSelectedItem()).map(i -> i.key().toString())
                .orElse(EMPTY));
        data.put("blankIfOdd", Boolean.toString(blankIfOdd.isSelected()));
        data.put("footer", Boolean.toString(footer.isSelected()));
        data.put("pageNormalization", Optional.ofNullable(pageNormalization.getSelectionModel().getSelectedItem())
                .map(i -> i.key().toString()).orElse(EMPTY));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("outline")).map(OutlinePolicy::valueOf)
                .flatMap(key -> this.outline.getItems().stream().filter(i -> i.key().equals(key)).findFirst())
                .ifPresent(this.outline.getSelectionModel()::select);

        Optional.ofNullable(data.get("acroForms")).map(AcroFormPolicy::valueOf)
                .flatMap(key -> this.acroForms.getItems().stream().filter(i -> i.key().equals(key)).findFirst())
                .ifPresent(this.acroForms.getSelectionModel()::select);

        Optional.ofNullable(data.get("toc")).map(ToCPolicy::valueOf)
                .flatMap(key -> this.toc.getItems().stream().filter(i -> i.key().equals(key)).findFirst())
                .ifPresent(this.toc.getSelectionModel()::select);

        var normalization = Optional.ofNullable(data.get("pageNormalization")).map(PageNormalizationPolicy::valueOf)
                .orElseGet(() -> {
                    //backward compatibility
                    if (Boolean.parseBoolean(data.get("normalize"))) {
                        return PageNormalizationPolicy.SAME_WIDTH_ORIENTATION_BASED;
                    }
                    return PageNormalizationPolicy.NONE;
                });
        this.pageNormalization.getItems().stream().filter(i -> i.key().equals(normalization)).findFirst()
                .ifPresent(this.pageNormalization.getSelectionModel()::select);

        blankIfOdd.setSelected(Boolean.parseBoolean(data.get("blankIfOdd")));
        footer.setSelected(Boolean.parseBoolean(data.get("footer")));
    }
}
