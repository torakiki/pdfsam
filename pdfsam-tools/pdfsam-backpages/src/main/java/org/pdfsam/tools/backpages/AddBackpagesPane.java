/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22/11/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.tools.backpages;

import javafx.geometry.HPos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import org.pdfsam.core.support.params.ConversionUtils;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.selection.single.SingleSelectionPane;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.Style;
import org.sejda.conversion.PdfFileSourceAdapter;
import org.sejda.conversion.exception.ConversionException;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.core.support.params.ConversionUtils.toPageRangeSet;
import static org.pdfsam.core.support.validation.Validators.positiveInteger;
import static org.pdfsam.core.support.validation.Validators.validEmpty;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * @author Andrea Vacondio
 */
public class AddBackpagesPane extends VBox
        implements TaskParametersBuildStep<AddBackpagesParametersBuilder>, RestorableView, ResettableView {

    private final SingleSelectionPane backpagesSourceField;
    private final ValidableTextField range = new ValidableTextField();
    private final ValidableTextField pace = new ValidableTextField();

    public AddBackpagesPane(String ownerModule) {
        super(Style.DEFAULT_SPACING);
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.VCONTAINER.css());
        backpagesSourceField = new SingleSelectionPane(ownerModule, false);
        this.backpagesSourceField.setPromptText(
                i18n().tr("Select or drag and drop the PDF whose pages will be repeated"));
        this.backpagesSourceField.setId("backpagesSource");
        this.backpagesSourceField.setAccessibleText(i18n().tr("Repeating PDF file"));
        this.backpagesSourceField.setAccessibleRoleDescription(i18n().tr("Repeating PDF file"));

        var options = new GridPane();
        options.getStyleClass().addAll(Style.GRID.css());

        var repeatLabel = new Label(i18n().tr("Repeat these pages:"));
        repeatLabel.setLabelFor(range);
        GridPane.setValignment(repeatLabel, VPos.BOTTOM);
        GridPane.setHalignment(repeatLabel, HPos.LEFT);
        options.add(repeatLabel, 0, 0);

        var rangeHelp = """
                %s.
                %s
                """.formatted(
                i18n().tr("Comma separated page numbers or ranges to be repeated (ex: 2 or 5-23 or 2,5-7,12-)"),
                i18n().tr("Leave it empty to use all the pages of the PDF file"));
        this.range.setOnEnterValidation(true);
        this.range.setEnableInvalidStyle(true);
        this.range.setPromptText(i18n().tr("Pages to repeat (ex: 2 or 5-23 or 2,5-7,12-)"));
        this.range.setValidator(validEmpty(v -> {
            try {
                return !toPageRangeSet(this.range.getText()).isEmpty();
            } catch (ConversionException e) {
                return false;
            }
        }));
        this.range.setErrorMessage(i18n().tr("Invalid page ranges"));
        this.range.setId("selectedBackpages");
        this.range.setPrefWidth(350);
        this.range.setAccessibleHelp(rangeHelp);
        GridPane.setValignment(range, VPos.BOTTOM);
        GridPane.setHalignment(range, HPos.LEFT);
        options.add(range, 1, 0);

        var rangeHelpIcon = helpIcon(rangeHelp);
        GridPane.setValignment(rangeHelpIcon, VPos.CENTER);
        options.add(rangeHelpIcon, 2, 0);

        var paceLabel = new Label(i18n().tr("Repeat every \"n\" pages:"));
        paceLabel.setLabelFor(pace);
        GridPane.setValignment(paceLabel, VPos.BOTTOM);
        GridPane.setHalignment(paceLabel, HPos.LEFT);
        options.add(paceLabel, 0, 1);

        var paceHelp = i18n().tr("Repeat the selected pages every \"n\" pages of the original document");
        this.pace.setText("1");
        this.pace.setOnEnterValidation(true);
        this.pace.setEnableInvalidStyle(true);
        this.pace.setPromptText(i18n().tr("Number of pages"));
        this.pace.setValidator(validEmpty(positiveInteger()));
        this.pace.setErrorMessage(i18n().tr("Invalid page number"));
        this.pace.setId("repeatPace");
        this.pace.setPrefWidth(350);
        this.pace.setAccessibleHelp(paceHelp);
        GridPane.setValignment(pace, VPos.BOTTOM);
        GridPane.setHalignment(pace, HPos.LEFT);
        options.add(pace, 1, 1);

        var stepHelpIcon = helpIcon(paceHelp);
        GridPane.setValignment(stepHelpIcon, VPos.CENTER);
        options.add(stepHelpIcon, 2, 1);

        this.getChildren().addAll(backpagesSourceField, options);
    }

    @Override
    public void apply(AddBackpagesParametersBuilder builder, Consumer<String> onError) {
        var textField = backpagesSourceField.getField().getTextField();
        textField.validate();
        if (textField.getValidationState() == FXValidationSupport.ValidationState.VALID) {
            builder.backPagesSource(new PdfFileSourceAdapter(textField.getText()).getPdfFileSource());
        } else {
            onError.accept(i18n().tr("A .pdf extension is required for the repeating file"));
        }

        this.pace.validate();
        if (pace.getValidationState() == FXValidationSupport.ValidationState.VALID) {
            if (isNotBlank(pace.getText())) {
                builder.step(Integer.parseInt(pace.getText()));
            } else {
                builder.step(1);
            }
        } else {
            onError.accept(i18n().tr("The number of pages must be a positive number"));
        }

        this.range.validate();
        if (this.range.getValidationState() == FXValidationSupport.ValidationState.VALID) {
            if (isNotBlank(range.getText())) {
                try {
                    builder.ranges(ConversionUtils.toPageRangeSet(this.range.getText()));
                } catch (ConversionException e) {
                    onError.accept(e.getMessage());
                }
            }
        } else {
            onError.accept(i18n().tr("Invalid page ranges"));
        }

    }

    @Override
    public void resetView() {
        this.range.setText("");
        this.pace.setText("1");
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        backpagesSourceField.saveStateTo(data);
        data.put("range.field", defaultString(range.getText()));
        data.put("pace.field", defaultString(pace.getText()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        backpagesSourceField.restoreStateFrom(data);
        range.setText(Optional.ofNullable(data.get("range.field")).orElse(EMPTY));
        pace.setText(Optional.ofNullable(data.get("pace.field")).orElse(EMPTY));
    }
}
