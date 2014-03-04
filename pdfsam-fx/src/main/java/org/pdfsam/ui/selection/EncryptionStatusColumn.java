/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/feb/2014
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

import java.util.Comparator;

import javafx.beans.value.ObservableValue;
import javafx.geometry.Point2D;
import javafx.scene.Scene;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;
import javafx.util.Callback;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.pdf.EncryptionStatus;

import de.jensd.fx.fontawesome.AwesomeDude;
import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * Definition of the {@link EncryptionStatus} column of the selection table
 * 
 * @author Andrea Vacondio
 *
 */
class EncryptionStatusColumn implements SelectionTableColumn<EncryptionStatus>, ModuleOwned {
    private String ownerModule = StringUtils.EMPTY;

    EncryptionStatusColumn(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
    }

    public String getOwnerModule() {
        return ownerModule;
    }

    public String getColumnTitle() {
        return null;
    }

    @Override
    public ObservableValue<EncryptionStatus> getObservableValue(SelectionTableRowData data) {
        return data.getDocumentDescriptor().encryptionStatusProperty();
    }

    @Override
    public String getTextValue(EncryptionStatus item) {
        return (item != null) ? item.name() : "";
    }

    @Override
    public Callback<TableColumn<SelectionTableRowData, EncryptionStatus>, TableCell<SelectionTableRowData, EncryptionStatus>> cellFactory() {
        return new Callback<TableColumn<SelectionTableRowData, EncryptionStatus>, TableCell<SelectionTableRowData, EncryptionStatus>>() {
            public TableCell<SelectionTableRowData, EncryptionStatus> call(
                    TableColumn<SelectionTableRowData, EncryptionStatus> param) {
                TableCell<SelectionTableRowData, EncryptionStatus> cell = new TableCell<SelectionTableRowData, EncryptionStatus>() {
                    private boolean canBeDecrypted = false;
                    {
                        addEventFilter(MouseEvent.MOUSE_CLICKED, (e) -> {
                            if (canBeDecrypted) {
                                showPasswordRequest();
                            }
                        });
                    }

                    public void showPasswordRequest() {
                        Scene scene = getScene();
                        if (scene != null) {
                            Window owner = scene.getWindow();
                            if (owner != null && owner.isShowing()) {
                                Point2D nodeCoord = this.localToScene(this.getWidth() / 2, this.getHeight() / 1.5);
                                double anchorX = Math.round(owner.getX() + scene.getX() + nodeCoord.getX() + 2);
                                double anchorY = Math.round(owner.getY() + scene.getY() + nodeCoord.getY() - 2);
                                PasswordFieldPopup popup = new PasswordFieldPopup((SelectionTableRowData) getTableRow()
                                        .getItem(), getOwnerModule());
                                popup.show(this, anchorX, anchorY);
                            }
                        }
                    }

                    @Override
                    public void updateItem(final EncryptionStatus item, boolean empty) {
                        super.updateItem(item, empty);
                        canBeDecrypted = false;
                        if (item != null) {
                            canBeDecrypted = item.canBeDecrypted();
                            switch (item) {
                            case ENCRYPTED:
                                setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.LOCK));
                                setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                                        "This document is encrypted, double click to provide a password.")));
                                break;
                            case DECRYPTION_REQUESTED:
                                setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.SPINNER));
                                setTooltip(null);
                                break;
                            case DECRYPTED_WITH_USER_PWD:
                                setGraphic(AwesomeDude.createIconLabel(AwesomeIcon.UNLOCK));
                                setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                                        "Valid user password provided.")));
                                break;
                            default:
                                setGraphic(null);
                                setTooltip(null);
                                break;
                            }
                        } else {
                            setGraphic(null);
                            setTooltip(null);
                        }
                    }

                };
                cell.setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
                cell.setText("");
                return cell;
            }
        };
    }

    public TableColumn<SelectionTableRowData, EncryptionStatus> getTableColumn() {
        TableColumn<SelectionTableRowData, EncryptionStatus> tableColumn = new TableColumn<>(getColumnTitle());
        tableColumn.setCellFactory(cellFactory());
        tableColumn.setCellValueFactory(cellValueFactory());
        tableColumn.setSortable(false);
        tableColumn.setMaxWidth(22);
        tableColumn.setMinWidth(22);
        return tableColumn;
    }

    public Comparator<EncryptionStatus> comparator() {
        // not used
        return null;
    }

}
