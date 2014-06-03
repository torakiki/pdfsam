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
import javafx.animation.Animation;
import javafx.animation.Interpolator;
import javafx.animation.RotateTransition;
import javafx.beans.property.SimpleObjectProperty;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;
import javafx.util.Duration;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.PdfDescriptorLoadingStatus;
import org.pdfsam.pdf.PdfDocumentDescriptorProvider;
import org.pdfsam.ui.event.ShowStageRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Component adding support for showing {@link PdfDescriptorLoadingStatus} icons and a popup asking the user to input a password if the document is encrypted
 * 
 * @author Andrea Vacondio
 *
 */
public class LoadingStatusIndicator extends Label implements ModuleOwned {
    private static final Logger LOG = LoggerFactory.getLogger(LoadingStatusIndicator.class);

    private SimpleObjectProperty<PdfDescriptorLoadingStatus> loadingStatus = new SimpleObjectProperty<>(
            PdfDescriptorLoadingStatus.INITIAL);
    private String ownerModule = StringUtils.EMPTY;
    private PdfDocumentDescriptorProvider descriptorProvider;
    private PasswordFieldPopup popup;
    private final RotateTransition rotate = new RotateTransition(Duration.seconds(2), this);

    public LoadingStatusIndicator(PdfDocumentDescriptorProvider descriptorProvider, String ownerModule) {
        requireNotNull(descriptorProvider,
                "Cannot create LoadingStatusIndicator with a null PdfDocumentDescriptorProvider");
        this.ownerModule = defaultString(ownerModule);
        this.popup = new PasswordFieldPopup(getOwnerModule());
        this.descriptorProvider = descriptorProvider;
        this.addEventFilter(MouseEvent.MOUSE_CLICKED, (e) -> {
            if (loadingStatus.get() == ENCRYPTED) {
                showPasswordRequest();
            } else if (loadingStatus.get() == WITH_ERRORS) {
                eventStudio().broadcast(new ShowStageRequest(), "LogStage");
            }
        });
        loadingStatus.addListener((o, oldVal, newVal) -> updateIndicator(newVal));
        this.getStyleClass().addAll("encryption-status");
        rotate.setByAngle(360);
        rotate.setCycleCount(Animation.INDEFINITE);
        rotate.setInterpolator(Interpolator.LINEAR);
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
     * @param loadingStatus
     */
    public void setLoadingStatus(final PdfDescriptorLoadingStatus loadingStatus) {
        if (loadingStatus != null) {
            this.loadingStatus.set(loadingStatus);
        } else {
            noIndicator();
        }
    }

    private void updateIndicator(final PdfDescriptorLoadingStatus loadingStatus) {
        LOG.trace("Updating idicator for new status {}", loadingStatus);
        switch (loadingStatus) {
        case ENCRYPTED:
            rotate.stop();
            indicator(
                    AwesomeIcon.LOCK,
                    DefaultI18nContext.getInstance().i18n(
                            "This document is encrypted, double click to provide a password."));
            break;
        case REQUESTED:
        case LOADING:
            indicator(AwesomeIcon.GEAR, null);
            rotate.play();
            break;
        case LOADED_WITH_USER_PWD_DECRYPTION:
            rotate.stop();
            indicator(AwesomeIcon.UNLOCK, DefaultI18nContext.getInstance().i18n("Valid user password provided."));
            break;
        case WITH_ERRORS:
            rotate.stop();
            indicator(AwesomeIcon.WARNING, null);
            break;
        case LOADED:
        default:
            // noIndicator();
            break;
        }
    }

    private void indicator(AwesomeIcon icon, String tooltip) {
        setRotate(0);
        setGraphic(AwesomeDude.createIconLabel(icon));
        if (isNotBlank(tooltip)) {
            this.setTooltip(new Tooltip(tooltip));
        } else {
            this.setTooltip(null);
        }
    }

    private void noIndicator() {
        rotate.stop();
        this.setGraphic(null);
        this.setTooltip(null);
    }
}
