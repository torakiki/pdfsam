/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/mag/2014
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
package org.pdfsam.model.pdf;

import org.kordamp.ikonli.Ikon;
import org.kordamp.ikonli.unicons.UniconsLine;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Possible loading status for a descriptor
 *
 * @author Andrea Vacondio
 */
public enum PdfDescriptorLoadingStatus {
    INITIAL,
    REQUESTED(UniconsLine.CLOCK, ""),
    LOADING(UniconsLine.ANGLE_RIGHT, ""),
    LOADED,
    LOADED_WITH_USER_PWD_DECRYPTION(UniconsLine.UNLOCK, i18n().tr("Valid user password provided.")),
    ENCRYPTED(UniconsLine.LOCK, i18n().tr("This document is encrypted, click to provide a password."), "with-warnings"),
    WITH_ERRORS(UniconsLine.EXCLAMATION_CIRCLE, i18n().tr("An error has occurred, click for more details."),
            "with-errors");

    static {
        INITIAL.setValidDestinationStatus(REQUESTED, WITH_ERRORS);
        ENCRYPTED.setValidDestinationStatus(REQUESTED, WITH_ERRORS);
        LOADING.setValidDestinationStatus(LOADED, LOADED_WITH_USER_PWD_DECRYPTION, ENCRYPTED, WITH_ERRORS);
        REQUESTED.setValidDestinationStatus(LOADING, WITH_ERRORS);
    }

    private final Set<PdfDescriptorLoadingStatus> validNext = new HashSet<>();
    private final Ikon icon;
    private final String description;
    private final String style;

    PdfDescriptorLoadingStatus() {
        this(null, "");
    }

    PdfDescriptorLoadingStatus(Ikon icon, String description) {
        this(icon, description, "");
    }

    PdfDescriptorLoadingStatus(Ikon icon, String description, String style) {
        this.icon = icon;
        this.description = defaultString(description);
        this.style = defaultString(style);
    }

    public Ikon getIcon() {
        return icon;
    }

    public String getDescription() {
        return description;
    }

    public String getStyle() {
        return style;
    }

    private void setValidDestinationStatus(PdfDescriptorLoadingStatus... canMoveTo) {
        validNext.addAll(Arrays.asList(canMoveTo));
    }

    /**
     * @param dest
     * @return true if the {@link PdfDescriptorLoadingStatus} can move to the given destination status
     */
    public boolean canMoveTo(PdfDescriptorLoadingStatus dest) {
        return validNext.contains(dest);
    }

    /**
     * Moves the current status to the destination one if allowed
     * 
     * @param dest
     * @return the destination status
     */
    public PdfDescriptorLoadingStatus moveTo(PdfDescriptorLoadingStatus dest) {
        if (canMoveTo(dest)) {
            return dest;
        }
        throw new IllegalStateException("Cannot move status from " + this + " to " + dest);
    }

    public boolean isFinal() {
        return validNext.isEmpty();
    }
}
