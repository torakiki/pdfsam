/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.merge;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.pdfsam.support.KeyStringValueItem.keyEmptyValue;
import static org.pdfsam.support.KeyStringValueItem.keyValue;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.model.outline.OutlinePolicy;
import org.sejda.model.pdf.form.AcroFormPolicy;
import org.sejda.model.toc.ToCPolicy;

import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;

/**
 * Panel for the Merge options
 * 
 * @author Andrea Vacondio
 *
 */
class MergeOptionsPane extends VBox
        implements TaskParametersBuildStep<MergeParametersBuilder>, RestorableView, ResettableView {
    private ComboBox<KeyStringValueItem<AcroFormPolicy>> acroForms = new ComboBox<>();
    private CheckBox blankIfOdd;
    private CheckBox footer;
    private CheckBox normalize;
    private ComboBox<KeyStringValueItem<OutlinePolicy>> outline = new ComboBox<>();
    private ComboBox<KeyStringValueItem<ToCPolicy>> toc = new ComboBox<>();

    MergeOptionsPane() {
        super(5);
        I18nContext i18n = I18nContext.getInstance();
        blankIfOdd = new CheckBox(i18n.i18n("Add a blank page if page number is odd"));
        blankIfOdd.setGraphic(helpIcon(
                i18n.i18n("Adds a blank page after each merged document if the document has an odd number of pages")));
        blankIfOdd.getStyleClass().addAll(Style.WITH_HELP.css());
        blankIfOdd.setId("blankIfOddCheck");

        footer = new CheckBox(i18n.i18n("Add a footer"));
        footer.setGraphic(helpIcon(i18n.i18n("Adds a page footer with the name of the file the page belonged to.")));
        footer.getStyleClass().addAll(Style.WITH_HELP.css());
        footer.setId("footerCheck");

        normalize = new CheckBox(i18n.i18n("Normalize pages size"));
        normalize.setGraphic(helpIcon(i18n.i18n("Resizes all pages to have the same width as the first page.")));
        normalize.getStyleClass().addAll(Style.WITH_HELP.css());
        normalize.setId("normalizeCheck");

        GridPane options = new GridPane();

        acroForms.getItems().add(keyValue(AcroFormPolicy.MERGE, i18n.i18n("Merge fields")));
        acroForms.getItems().add(
                keyValue(AcroFormPolicy.MERGE_RENAMING_EXISTING_FIELDS, i18n.i18n("Merge renaming existing fields")));
        acroForms.getItems().add(keyValue(AcroFormPolicy.FLATTEN, i18n.i18n("Flatten")));
        acroForms.getItems().add(keyValue(AcroFormPolicy.DISCARD, i18n.i18n("Discard forms")));
        acroForms.setId("acroFormsCombo");
        options.add(new Label(i18n.i18n("Interactive forms (AcroForms):")), 0, 0);
        acroForms.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(acroForms, 1, 0);
        options.add(helpIcon(i18n.i18n("What to do in case one or more input documents contain Acro Forms")), 2, 0);

        outline.getItems().add(keyValue(OutlinePolicy.RETAIN, i18n.i18n("Retain bookmarks")));
        outline.getItems().add(keyValue(OutlinePolicy.DISCARD, i18n.i18n("Discard bookmarks")));
        outline.getItems().add(
                keyValue(OutlinePolicy.ONE_ENTRY_EACH_DOC, i18n.i18n("Create one entry for each merged document")));
        outline.getItems().add(keyValue(OutlinePolicy.RETAIN_AS_ONE_ENTRY,
                i18n.i18n("Retain bookmarks as one entry for each merged document")));

        outline.setId("outlineCombo");
        options.add(new Label(i18n.i18n("Bookmarks handling:")), 0, 1);
        outline.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(outline, 1, 1);
        options.add(helpIcon(i18n.i18n("What to do in case one or more input documents contain bookmarks")), 2, 1);

        toc.getItems().add(keyValue(ToCPolicy.NONE, i18n.i18n("Don't generate")));
        toc.getItems().add(keyValue(ToCPolicy.FILE_NAMES, i18n.i18n("Generate from file names")));
        toc.getItems().add(keyValue(ToCPolicy.DOC_TITLES, i18n.i18n("Generate from documents titles")));

        toc.setId("tocCombo");
        options.add(new Label(i18n.i18n("Table of contents:")), 0, 2);
        toc.setMaxWidth(Double.POSITIVE_INFINITY);
        options.add(toc, 1, 2);
        options.add(helpIcon(i18n.i18n("Set if a table of contents should be added to the generated PDF document")), 2,
                2);
        options.getStyleClass().addAll(Style.GRID.css());

        getStyleClass().addAll(Style.CONTAINER.css());
        resetView();
        getChildren().addAll(blankIfOdd, footer, normalize, options);
    }

    @Override
    public void resetView() {
        blankIfOdd.setSelected(false);
        footer.setSelected(false);
        normalize.setSelected(false);
        acroForms.getSelectionModel().selectFirst();
        outline.getSelectionModel().selectFirst();
        toc.getSelectionModel().selectFirst();
    }

    @Override
    public void apply(MergeParametersBuilder builder, Consumer<String> onError) {
        builder.outlinePolicy(outline.getSelectionModel().getSelectedItem().getKey());
        builder.acroFormsPolicy(acroForms.getSelectionModel().getSelectedItem().getKey());
        builder.tocPolicy(toc.getSelectionModel().getSelectedItem().getKey());
        builder.blankPageIfOdd(blankIfOdd.isSelected());
        builder.footer(footer.isSelected());
        builder.normalize(normalize.isSelected());
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("outline", Optional.ofNullable(outline.getSelectionModel().getSelectedItem())
                .map(i -> i.getKey().toString()).orElse(EMPTY));
        data.put("acroForms", Optional.ofNullable(acroForms.getSelectionModel().getSelectedItem())
                .map(i -> i.getKey().toString()).orElse(EMPTY));
        data.put("toc", Optional.ofNullable(toc.getSelectionModel().getSelectedItem()).map(i -> i.getKey().toString())
                .orElse(EMPTY));
        data.put("blankIfOdd", Boolean.toString(blankIfOdd.isSelected()));
        data.put("footer", Boolean.toString(footer.isSelected()));
        data.put("normalize", Boolean.toString(normalize.isSelected()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("outline")).map(OutlinePolicy::valueOf).map(r -> keyEmptyValue(r))
                .ifPresent(r -> this.outline.getSelectionModel().select(r));
        Optional.ofNullable(data.get("acroForms")).map(AcroFormPolicy::valueOf).map(r -> keyEmptyValue(r))
                .ifPresent(r -> this.acroForms.getSelectionModel().select(r));
        Optional.ofNullable(data.get("toc")).map(ToCPolicy::valueOf).map(r -> keyEmptyValue(r))
                .ifPresent(r -> this.toc.getSelectionModel().select(r));
        blankIfOdd.setSelected(Boolean.valueOf(data.get("blankIfOdd")));
        footer.setSelected(Boolean.valueOf(data.get("footer")));
        normalize.setSelected(Boolean.valueOf(data.get("normalize")));
    }
}
