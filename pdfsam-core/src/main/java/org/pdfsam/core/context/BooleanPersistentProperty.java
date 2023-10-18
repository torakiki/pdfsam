package org.pdfsam.core.context;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 18/09/22
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

import java.util.function.Supplier;

import static org.pdfsam.core.ConfigurableSystemProperty.CHECK_FOR_NEWS_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.CHECK_FOR_UPDATES_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.DISCARD_BOOKMARKS_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.DONATE_NOTIFICATION_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.FETCH_PREMIUM_MODULES_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.OVERWRITE_OUTPUT_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.PDF_COMPRESSION_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.PLAY_SOUNDS_PROP;
import static org.pdfsam.core.ConfigurableSystemProperty.SMART_OUTPUT_PROP;

/**
 * @author Andrea Vacondio
 */
public enum BooleanPersistentProperty implements PersistentProperty<Boolean> {
    PLAY_SOUNDS(() -> Boolean.parseBoolean(System.getProperty(PLAY_SOUNDS_PROP, Boolean.TRUE.toString()))),
    DONATION_NOTIFICATION(
            () -> Boolean.parseBoolean(System.getProperty(DONATE_NOTIFICATION_PROP, Boolean.TRUE.toString()))),
    PREMIUM_MODULES(
            () -> Boolean.parseBoolean(System.getProperty(FETCH_PREMIUM_MODULES_PROP, Boolean.TRUE.toString()))),
    SMART_OUTPUT(() -> Boolean.parseBoolean(System.getProperty(SMART_OUTPUT_PROP, Boolean.TRUE.toString()))),
    CHECK_UPDATES(() -> Boolean.parseBoolean(System.getProperty(CHECK_FOR_UPDATES_PROP, Boolean.TRUE.toString()))),
    CHECK_FOR_NEWS(() -> Boolean.parseBoolean(System.getProperty(CHECK_FOR_NEWS_PROP, Boolean.TRUE.toString()))),
    OVERWRITE_OUTPUT(() -> Boolean.parseBoolean(System.getProperty(OVERWRITE_OUTPUT_PROP, Boolean.FALSE.toString()))),
    PDF_COMPRESSION_ENABLED(
            () -> Boolean.parseBoolean(System.getProperty(PDF_COMPRESSION_PROP, Boolean.TRUE.toString()))),
    CLEAR_CONFIRMATION(() -> Boolean.TRUE),
    SAVE_WORKSPACE_ON_EXIT(() -> Boolean.FALSE),
    SAVE_PWD_IN_WORKSPACE(() -> Boolean.FALSE),
    SIDEBAR_EXPANDED_STATE(() -> Boolean.TRUE),
    DISCARD_BOOKMARKS(() -> Boolean.parseBoolean(System.getProperty(DISCARD_BOOKMARKS_PROP, Boolean.FALSE.toString())));

    private final Supplier<Boolean> defaultSupplier;

    BooleanPersistentProperty(Supplier<Boolean> supplier) {
        this.defaultSupplier = supplier;
    }

    @Override
    public String key() {
        return this.name().toLowerCase();
    }

    @Override
    public Supplier<Boolean> defaultSupplier() {
        return defaultSupplier;
    }
}