/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25/ott/2013
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
package org.pdfsam.ui.components.io;

import javafx.css.PseudoClass;
import javafx.scene.AccessibleAttribute;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Tooltip;
import javafx.scene.input.Clipboard;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.Style;

import java.io.File;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * {@link ValidableTextField} with attached a browse button to let the user select a file
 *
 * @author Andrea Vacondio
 */
abstract class BrowsableField extends HBox implements RestorableView {
    private static final PseudoClass SELECTED_PSEUDOCLASS_STATE = PseudoClass.getPseudoClass("selected");

    private final Button browseButton;
    private final ValidableTextField textField = new ValidableTextField() {

        @Override
        public void paste() {
            Clipboard clipboard = Clipboard.getSystemClipboard();
            if (clipboard.hasString()) {
                String text = clipboard.getString();
                if (text.length() > 2 && text.charAt(0) == '"' && text.charAt(text.length() - 1) == '"') {
                    replaceSelection(text.substring(1, text.length() - 1));
                } else {
                    super.paste();
                }
            }
        }

    };
    private final HBox validableContainer;
    private String browseWindowTitle = i18n().tr("Select");

    public BrowsableField() {
        this(new Button(i18n().tr("Browse")));
        browseButton.getStyleClass().addAll(Style.BROWSE_BUTTON.css());
        browseButton.prefHeightProperty().bind(textField.heightProperty());
        browseButton.setMaxHeight(USE_PREF_SIZE);
        browseButton.setMinHeight(USE_PREF_SIZE);
        getChildren().add(browseButton);
    }

    public BrowsableField(Button browseButton) {
        this.browseButton = browseButton;
        HBox.setHgrow(textField, Priority.ALWAYS);
        this.getStyleClass().add("browsable-field");
        validableContainer = new HBox(textField);
        validableContainer.getStyleClass().add("validable-container");
        textField.getStyleClass().add("validable-container-field");
        HBox.setHgrow(validableContainer, Priority.ALWAYS);
        textField.validProperty().addListener((o, oldValue, newValue) -> {
            if (newValue == FXValidationSupport.ValidationState.INVALID) {
                validableContainer.getStyleClass().addAll(Style.INVALID.css());
                textField.setAccessibleText(i18n().tr("Invalid value: {0}", textField.getPromptText()));
                textField.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT);
            } else {
                validableContainer.getStyleClass().removeAll(Style.INVALID.css());
                textField.setAccessibleText(textField.getPromptText());
                textField.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT);
            }
        });
        textField.focusedProperty().addListener(
                (o, oldVal, newVal) -> validableContainer.pseudoClassStateChanged(SELECTED_PSEUDOCLASS_STATE, newVal));
        getChildren().add(validableContainer);
    }

    /**
     * @return the internal {@link ValidableTextField}
     */
    public ValidableTextField getTextField() {
        return textField;
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put(defaultString(getId()) + "browsableField", defaultString(textField.getText()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        textField.setText(Optional.ofNullable(data.get(defaultString(getId()) + "browsableField")).orElse(EMPTY));
    }

    public final void setGraphic(Node value) {
        validableContainer.getChildren().clear();
        if (value != null) {
            validableContainer.getChildren().add(value);
        }
        validableContainer.getChildren().add(textField);
    }

    /**
     * Sets both prompt text and accessible text on the text field, keeping them in sync.
     */
    protected void setFieldPromptAndAccessibleText(String text) {
        textField.setPromptText(text);
        textField.setAccessibleText(text);
    }

    /**
     * Sets accessible text and tooltip on the browse button for screen reader context.
     */
    protected void setBrowseButtonAccessibleText(String text) {
        browseButton.setAccessibleText(text);
        browseButton.setTooltip(new Tooltip(text));
    }

    Button getBrowseButton() {
        return browseButton;
    }

    String getBrowseWindowTitle() {
        return browseWindowTitle;
    }

    public void setBrowseWindowTitle(String browseWindowTitle) {
        this.browseWindowTitle = browseWindowTitle;
    }

    /**
     * Set the text from the given input file
     */
    abstract void setTextFromFile(File inputFile);

    /**
     * Set the text from the given input file
     */
    public void setTextFromFile(Path inputFile) {
        if (Objects.nonNull(inputFile)) {
            this.setTextFromFile(inputFile.toFile());
        }
    }
}
