/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 16/mag/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.selection;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.ENCRYPTED;
import static org.pdfsam.pdf.PdfDescriptorLoadingStatus.WITH_ERRORS;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.ui.event.ShowStageRequest;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Component adding support for showing {@link PdfDescriptorLoadingStatus} icons and a popup asking the user to input a password if the document is encrypted
 * 
 * @author Andrea Vacondio
 *
 */
public class LoadingStatusIndicator extends Label implements ModuleOwned {

    private PdfDescriptorLoadingStatus encryptionStatus = PdfDescriptorLoadingStatus.REQUESTED;
    private String ownerModule = StringUtils.EMPTY;
    private PdfDocumentDescriptorProvider descriptorProvider;
    private PasswordFieldPopup popup;

    public LoadingStatusIndicator(PdfDocumentDescriptorProvider descriptorProvider, String ownerModule) {
        requireNotNull(descriptorProvider,
                "Cannot create EncryptionStatusSupport with a null PdfDocumentDescriptorProvider");
        this.ownerModule = defaultString(ownerModule);
        this.popup = new PasswordFieldPopup(getOwnerModule());
        this.descriptorProvider = descriptorProvider;
        this.addEventFilter(MouseEvent.MOUSE_CLICKED, (e) -> {
            if (encryptionStatus == ENCRYPTED) {
                showPasswordRequest();
            } else if (encryptionStatus == WITH_ERRORS) {
                eventStudio().broadcast(new ShowStageRequest(), "LogStage");
            }
        });
        this.getStyleClass().add("encryption-status");
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    /**
     * Show a password request right below the wrapped Control
     */
    public void showPasswordRequest() {
        Scene scene = this.getScene();
        if (scene != null) {
            Window owner = scene.getWindow();
            if (owner != null && owner.isShowing()) {
                Point2D nodeCoord = this.localToScene(this.getWidth() / 2, this.getHeight() / 1.5);
                double anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
                double anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() + 2);
                popup.showFor(descriptorProvider.getPdfDocumentDescriptor(), this, anchorX, anchorY);
            }
        }
    }

    /**
     * Updates the loading status
     * 
     * @param encryptionStatus
     */
    public void updateLoadingStatus(final PdfDescriptorLoadingStatus encryptionStatus) {
        if (encryptionStatus != null) {
            this.encryptionStatus = encryptionStatus;
            switch (encryptionStatus) {
            case ENCRYPTED:
                indicator(
                        AwesomeIcon.LOCK,
                        DefaultI18nContext.getInstance().i18n(
                                "This document is encrypted, double click to provide a password."));
                break;
            case REQUESTED:
            case LOADING:
                indicator(AwesomeIcon.SPINNER, null);
                break;
            case LOADED_WITH_USER_PWD_DECRYPTION:
                indicator(AwesomeIcon.UNLOCK, DefaultI18nContext.getInstance().i18n("Valid user password provided."));
                break;
            case WITH_ERRORS:
                indicator(AwesomeIcon.WARNING, null);
                break;
            case LOADED:
            default:
                noIndicator();
                break;
            }
        } else {
            noIndicator();
        }
    }

    private void indicator(AwesomeIcon icon, String tooltip) {
        this.setGraphic(AwesomeDude.createIconLabel(icon));
        if (isNotBlank(tooltip)) {
            this.setTooltip(new Tooltip(tooltip));
        } else {
            this.setTooltip(null);
        }
    }

    private void noIndicator() {
        this.setGraphic(null);
        this.setTooltip(null);
    }
}
