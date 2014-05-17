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
import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.Labeled;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.EncryptionStatus;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Component adding support for showing {@link EncryptionStatus} icons and a popup asking the user to input a password
 * 
 * @author Andrea Vacondio
 *
 */
public class EncryptionStatusSupport implements ModuleOwned {

    private EncryptionStatus encryptionStatus = EncryptionStatus.NOT_ENCRYPTED;
    private Labeled indicator;
    private String ownerModule = StringUtils.EMPTY;
    private PdfDocumentDescriptorProvider descriptorProvider;

    public EncryptionStatusSupport(PdfDocumentDescriptorProvider descriptorProvider, Labeled indicator,
            String ownerModule) {
        requireNotNull(indicator, "Cannot create EncryptionStatusSupport with a null indicator");
        requireNotNull(descriptorProvider,
                "Cannot create EncryptionStatusSupport with a null PdfDocumentDescriptorProvider");
        this.ownerModule = defaultString(ownerModule);
        this.indicator = indicator;
        this.descriptorProvider = descriptorProvider;
        this.indicator.addEventFilter(MouseEvent.MOUSE_CLICKED, (e) -> {
            if (encryptionStatus.canBeDecrypted()) {
                showPasswordRequest();
            }
        });
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    /**
     * Show a password request right below the wrapped Control
     */
    public void showPasswordRequest() {
        Scene scene = this.indicator.getScene();
        if (scene != null) {
            Window owner = scene.getWindow();
            if (owner != null && owner.isShowing()) {
                Point2D nodeCoord = this.indicator.localToScene(this.indicator.getWidth() / 2,
                        this.indicator.getHeight() / 1.5);
                double anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
                double anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() - 2);
                PasswordFieldPopup popup = new PasswordFieldPopup(descriptorProvider.getPdfDocumentDescriptor(),
                        getOwnerModule());
                popup.show(this.indicator, anchorX, anchorY);
            }
        }
    }

    /**
     * Updates the encryption status
     * 
     * @param encryptionStatus
     */
    public void updateEncryptionStatus(final EncryptionStatus encryptionStatus) {
        if (encryptionStatus != null) {
            this.encryptionStatus = encryptionStatus;
            switch (encryptionStatus) {
            case ENCRYPTED:
                this.indicator.setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.LOCK));
                this.indicator.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                        "This document is encrypted, double click to provide a password.")));
                break;
            case DECRYPTION_REQUESTED:
                this.indicator.setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.SPINNER));
                this.indicator.setTooltip(null);
                break;
            case DECRYPTED_WITH_USER_PWD:
                this.indicator.setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.UNLOCK));
                this.indicator.setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                        "Valid user password provided.")));
                break;
            default:
                this.indicator.setGraphic(null);
                this.indicator.setTooltip(null);
                break;
            }
        } else {
            this.indicator.setGraphic(null);
            this.indicator.setTooltip(null);
        }
    }
}
