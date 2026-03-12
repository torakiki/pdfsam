/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/mar/2014
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
package org.pdfsam.ui.components.selection;

import javafx.application.Platform;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.PasswordField;
import javafx.scene.control.PopupControl;
import javafx.scene.control.Skin;
import javafx.scene.layout.HBox;
import org.apache.commons.lang3.StringUtils;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.model.pdf.PdfDocumentDescriptor;
import org.pdfsam.model.pdf.PdfLoadRequest;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.ui.components.support.Style;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * A popup displaying a password field to let the user input a document password
 *
 * @author Andrea Vacondio
 */
public class PasswordFieldPopup extends PopupControl implements ToolBound {
    private String toolBinding = StringUtils.EMPTY;
    private final PasswordFieldPopupContent content = new PasswordFieldPopupContent();
    private PdfDocumentDescriptor[] pdfDescriptors;

    public PasswordFieldPopup(String toolBinding) {
        this.toolBinding = defaultString(toolBinding);
        getStyleClass().setAll("pdfsam-input-password");
        setAutoHide(true);
        setHideOnEscape(true);
        setAutoFix(true);
        eventStudio().addAnnotatedListeners(this);
    }

    @Override
    public String toolBinding() {
        return toolBinding;
    }

    PasswordFieldPopupContent getPopupContent() {
        return content;
    }

    @Override
    protected Skin<?> createDefaultSkin() {
        return new PasswordFieldPopupSkin(this);
    }

    public void showFor(Node owner, double anchorX, double anchorY, PdfDocumentDescriptor... pdfDescriptors) {
        this.pdfDescriptors = pdfDescriptors;
        this.show(owner, anchorX, anchorY);
        Platform.runLater(() -> content.passwordField.requestFocus());
    }

    /**
     * Panel containing fields to let the user enter a pdf document user password
     * 
     * @author Andrea Vacondio
     *
     */
    private class PasswordFieldPopupContent extends HBox {
        private final PasswordField passwordField = new PasswordField();

        public PasswordFieldPopupContent() {
            getStyleClass().setAll("pdfsam-input-password-content");
            passwordField.setPromptText(i18n().tr("Enter the user password"));
            passwordField.setAccessibleText(i18n().tr("PDF document password"));
            var doneButton = new Button(i18n().tr("Unlock"));
            var icon = FontIcon.of(UniconsLine.UNLOCK_ALT);
            icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
            doneButton.setGraphic(icon);
            doneButton.getStyleClass().addAll(Style.BUTTON.css());
            doneButton.prefHeightProperty().bind(passwordField.heightProperty());
            doneButton.setMaxHeight(USE_PREF_SIZE);
            doneButton.setMinHeight(USE_PREF_SIZE);
            doneButton.setOnAction(e -> requestLoad());
            passwordField.setOnAction(e -> requestLoad());
            getChildren().addAll(passwordField, doneButton);
        }

        public void requestLoad() {
            if (pdfDescriptors != null) {
                for (PdfDocumentDescriptor desc : pdfDescriptors) {
                    desc.setPassword(passwordField.getText());
                    var loadEvent = new PdfLoadRequest(toolBinding());
                    loadEvent.add(desc);
                    eventStudio().broadcast(loadEvent);
                }
            }
            passwordField.clear();
            hide();
        }
    }
}
