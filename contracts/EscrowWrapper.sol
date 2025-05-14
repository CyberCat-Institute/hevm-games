// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@lido-dual-governance/contracts/Escrow.sol";

/**
 * @title EscrowWrapper
 * @dev A wrapper contract that interacts with Lido's Escrow contract
 * and provides a function to lock stETH and return the locked amount
 */
contract EscrowWrapper {
    // stETH token address on mainnet
    address public constant STETH = 0xae7ab96520DE3A18E5e111B5EaAb095312D7fE84;

    // Reference to the Escrow contract
    Escrow public immutable escrow;

    /**
     * @dev Constructor that accepts the address of the Escrow contract
     * @param _escrow Address of the deployed Escrow contract
     */
    constructor(address payable _escrow) {
        require(_escrow != address(0), "Invalid escrow address");
        escrow = Escrow(_escrow);
    }

    /**
     * @dev Locks stETH in the Escrow contract and returns the amount locked
     * @param amount Amount of stETH to lock
     * @return The actual amount of stETH that was locked
     */
    function lockStETHAndReport(uint256 amount) external returns (uint256) {
        // Get the initial balance of the Escrow contract
        uint256 initialEscrowBalance = IERC20(STETH).balanceOf(address(escrow));

        // Ensure this contract has enough allowance to transfer stETH from the msg.sender
        require(IERC20(STETH).transferFrom(msg.sender, address(this), amount), "stETH transfer failed");

        // Approve the Escrow contract to spend stETH
        IERC20(STETH).approve(address(escrow), amount);

        // Lock the stETH in the Escrow contract
        escrow.lockStETH(amount);

        // Calculate the actual amount locked by checking the balance difference
        uint256 finalEscrowBalance = IERC20(STETH).balanceOf(address(escrow));
        uint256 amountLocked = finalEscrowBalance - initialEscrowBalance;

        return amountLocked;
    }

    /**
     * @dev This function allows direct locking of stETH without transferring through this contract
     * User must approve this contract to spend their stETH before calling this function
     * @param amount Amount of stETH to lock
     * @return The amount of stETH locked
     */
    function directLockStETH(uint256 amount) external returns (uint256) {
        // Get initial balance of escrow
        uint256 initialEscrowBalance = IERC20(STETH).balanceOf(address(escrow));

        // Transfer stETH from user to this contract
        require(IERC20(STETH).transferFrom(msg.sender, address(this), amount), "stETH transfer failed");

        // Approve escrow contract to spend the stETH
        IERC20(STETH).approve(address(escrow), amount);

        // Lock the stETH
        escrow.lockStETH(amount);

        // Calculate actual locked amount
        uint256 finalEscrowBalance = IERC20(STETH).balanceOf(address(escrow));
        uint256 amountLocked = finalEscrowBalance - initialEscrowBalance;

        return amountLocked;
    }

    /**
     * @dev Gets the total amount of stETH locked in the Escrow contract
     * @return The total amount of stETH locked
     */
    function getTotalLockedStETH() external view returns (uint256) {
        return IERC20(STETH).balanceOf(address(escrow));
    }
}
